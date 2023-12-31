use ice_parser::Parser;
use iiv_gen::{Generator, TokenType};
use lsp_types::{
    InitializeParams, InitializeResult, SemanticToken, SemanticTokenModifier, SemanticTokenType,
    SemanticTokens, SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions,
    SemanticTokensParams, SemanticTokensResult, SemanticTokensServerCapabilities,
    ServerCapabilities, ServerInfo, WorkDoneProgressOptions,
};
use serde::{Deserialize, Serialize};
use std::{
    error::Error,
    fs::File,
    io::{self, BufRead, BufReader, Read, Write},
};

fn err(message: &'static str) -> Box<dyn Error> {
    message.into()
}

fn add_ctx<E: std::error::Error>(message: &'static str) -> impl Fn(E) -> Box<dyn Error> {
    move |err| format!("{}: {}", message, &err.to_string()).into()
}

fn parse_option<'a>(option: &'static str, arg: &'a str) -> Result<&'a str, Box<dyn Error>> {
    if !arg.starts_with(option) {
        return Err(err("missing flag"));
    }
    Ok(&option[option.len()..])
}

fn main() -> Result<(), Box<dyn Error>> {
    let res = _main();
    if let Err(err) = &res {
        let mut f = File::create("C:/Users/tbuli/Apps/DEV/ice/ice-lsp/logs.txt")?;
        f.write_all(err.to_string().as_bytes())?;
    }
    res
}

fn _main() -> Result<(), Box<dyn Error>> {
    let mut args = std::env::args();
    args.next();
    let communication = args
        .next()
        .ok_or_else(|| err("missing communication type"))?;
    // let path = args.next().expect("no path given");

    match &communication[..] {
        "stdio" => {
            run_server(io::stdin(), io::stdout())?;
            Ok(())
        }
        "pipe" => {
            let pipe_name = args.next().ok_or_else(|| err("missing --pipe flag"))?;
            let _name = parse_option("--pipe=", &pipe_name)?;
            Err(err("pipe not supported"))
        }
        "socket" => {
            let port_name = args.next().ok_or_else(|| err("missing --port flag"))?;
            let _port = parse_option("--port=", &port_name)?;
            Err(err("socket not supported"))
        }
        _ => Err(err("Invalid communication type name")),
    }
}

fn run_server(reader: impl Read, mut writer: impl Write) -> Result<(), Box<dyn Error>> {
    let mut reader = BufReader::new(reader);
    let body = get_body(&mut reader)?;
    handle_initial_request(body, &mut writer)?;
    let mut f = File::options()
        .append(true)
        .open("C:/Users/tbuli/Apps/DEV/ice/ice-lsp/logs.txt")?;

    f.write_all("initial done\n".as_bytes())?;
    f.flush()?;
    loop {
        f.write_all("waiting for next\n".as_bytes())?;
        f.flush()?;
        let body = get_body(&mut reader)?;
        do_request(body, &mut writer)?;
    }
}

fn write_response(
    body: impl Serialize,
    id: u64,
    writer: &mut impl Write,
) -> Result<(), Box<dyn Error>> {
    let response = Response {
        jsonrpc: "2.0",
        id,
        result: body,
    };
    let response = serde_json::to_string(&response)?;
    writer.write_all(format!("Content-Length: {}\r\n\r\n", response.len()).as_bytes())?;
    writer.write_all(response.as_bytes())?;
    writer.flush()?;
    Ok(())
}

#[derive(Deserialize)]
struct Request<'r> {
    #[allow(dead_code)]
    jsonrpc: &'r str,
    id: Option<u64>,
    method: &'r str,
}

#[derive(Deserialize)]
struct Params<T> {
    params: T,
}

#[derive(Serialize)]
struct Response<'r, R> {
    jsonrpc: &'r str,
    id: u64,
    result: R,
}

fn get_body(reader: &mut impl BufRead) -> Result<String, Box<dyn Error>> {
    let mut content_type = None;
    let mut content_length = None;
    loop {
        let mut header = String::new();
        if reader.read_line(&mut header)? == 0 {
            return Err(err("recieved incomplete request"));
        };
        let mut f = File::options()
            .append(true)
            .open("C:/Users/tbuli/Apps/DEV/ice/ice-lsp/logs.txt")?;

        f.write_all("read header line\n".as_bytes())?;
        f.flush()?;
        drop(f);
        let header = header.trim();
        if header.is_empty() {
            break;
        }
        let (name, value) = header
            .split_once(": ")
            .ok_or_else(|| err("invalid header"))?;

        match name {
            "Content-Type" => {
                content_type = Some(value.to_string());
            }
            "Content-Length" => {
                content_length = Some(value.to_string());
            }
            _ => {}
        }
    }
    let len: usize = content_length
        .ok_or_else(|| err("missing content length"))?
        .parse()?;

    if let Some(ty) = &content_type {
        if ty != "utf8" {
            return Err(err("Invalid content type"));
        }
    }

    let mut body = vec![0; len];
    reader.read_exact(&mut body)?;
    let body = String::from_utf8(body)?;
    Ok(body)
}

fn handle_initial_request(body: String, writer: &mut impl Write) -> Result<(), Box<dyn Error>> {
    let request: Request = serde_json::from_str(&body)
        .map_err(|_| -> Box<dyn Error> { format!("invalid json: {}", &body).into() })?;

    let Some(id) = request.id else {
        return Ok(());
    };

    let mut f = File::options()
        .append(true)
        .open("C:/Users/tbuli/Apps/DEV/ice/ice-lsp/logs.txt")?;

    f.write_all(request.method.as_bytes())?;
    f.write_all("\n".as_bytes())?;

    // SemanticTokenType::;
    match request.method {
        "initialize" => {
            let _: Params<InitializeParams> =
                serde_json::from_str(&body).map_err(add_ctx("in initial request params"))?;
            let result = InitializeResult {
                capabilities: ServerCapabilities {
                    semantic_tokens_provider: Some(
                        SemanticTokensServerCapabilities::SemanticTokensOptions(
                            SemanticTokensOptions {
                                legend: SemanticTokensLegend {
                                    token_types: vec![
                                        SemanticTokenType::TYPE,
                                        SemanticTokenType::VARIABLE,
                                        SemanticTokenType::PROPERTY,
                                        SemanticTokenType::FUNCTION,
                                        SemanticTokenType::KEYWORD,
                                        SemanticTokenType::ENUM_MEMBER,
                                    ],
                                    token_modifiers: vec![SemanticTokenModifier::new(
                                        "controlFlow",
                                    )],
                                },
                                range: None,
                                work_done_progress_options: WorkDoneProgressOptions {
                                    work_done_progress: None,
                                },
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                        ),
                    ),

                    // hover_provider: Some(HoverProviderCapability::Simple(true)),
                    // {

                    //     dynamic_registration: None,
                    //     content_format: Some(vec![MarkupKind::PlainText]),
                    // }),
                    ..Default::default()
                },

                server_info: Some(ServerInfo {
                    name: "ice-lsp".to_string(),
                    version: None,
                }),
            };
            write_response(result, id, writer)?;
        }
        "exit" => std::process::exit(0),
        _ => return Err(err("invalid initial request")),
    }

    Ok(())
}

fn do_request(body: String, writer: &mut impl Write) -> Result<(), Box<dyn Error>> {
    let request: Request = serde_json::from_str(&body).map_err(add_ctx("in request body"))?;
    let Some(id) = request.id else {
        let mut f = File::options()
            .append(true)
            .open("C:/Users/tbuli/Apps/DEV/ice/ice-lsp/logs.txt")?;

        f.write_all(request.method.as_bytes())?;
        f.write_all("\n".as_bytes())?;
        return Ok(());
    };
    let mut f = File::options()
        .append(true)
        .open("C:/Users/tbuli/Apps/DEV/ice/ice-lsp/logs.txt")?;

    f.write_all(request.method.as_bytes())?;
    f.write_all("\n".as_bytes())?;
    match request.method {
        "textDocument/semanticTokens/full" => {
            let request: Params<SemanticTokensParams> =
                serde_json::from_str(&body).map_err(add_ctx("in request params"))?;
            let request = request.params;

            let name = request
                .text_document
                .uri
                .path_segments()
                .map(|segments| segments.last())
                .flatten()
                .unwrap_or("<unknown file>");

            let path = request
                .text_document
                .uri
                .to_file_path()
                .map_err(|_| err("invalid text url"))?;

            let file = File::open(path)?;

            let result = SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: semantic_highlight(file, name.to_string()),
            });

            write_response(result, id, writer)?;
        }
        "exit" => std::process::exit(0),
        _ => return Err(err("invalid request")),
    }

    Ok(())
}

fn semantic_highlight(file: File, name: String) -> Vec<SemanticToken> {
    let ctx = iiv::Ctx::new(file, name);

    let mut parser = Parser::new(&ctx, BufReader::new(&ctx.source.file));
    let mut generator = Generator::new(&ctx, true);

    let module = parser.parse_program();

    let _ = generator.emit_iiv(&[module]);

    let mut prev_line = 0;
    let mut prev_start = 0;

    let mut f = File::options()
        .append(true)
        .open("C:/Users/tbuli/Apps/DEV/ice/ice-lsp/logs.txt")
        .unwrap();

    f.write_all(format!("{:#?}", generator.index.0).as_bytes())
        .unwrap();
    f.flush().unwrap();
    drop(f);

    generator
        .index
        .0
        .as_mut()
        .unwrap()
        .sort_by_key(|a| a.0.begin_offset + a.0.begin_highlight_offset);

    generator
        .index
        .0
        .unwrap()
        .into_iter()
        .map(|(span, token_type)| {
            if span.first_line != prev_line {
                prev_start = 0;
            }

            let token = SemanticToken {
                token_modifiers_bitset: 0,
                token_type: match token_type {
                    TokenType::Type => 0,
                    TokenType::Variable => 1,
                    TokenType::Property => 2,
                    TokenType::Function => 3,
                    TokenType::Keyword => 4,
                    TokenType::Variant => 5,
                },
                length: span.end_highlight_offset - span.begin_highlight_offset,
                delta_start: span.begin_highlight_offset - prev_start,
                delta_line: span.first_line - prev_line,
            };
            prev_line = span.first_line;
            prev_start = span.begin_highlight_offset;
            token
        })
        .collect()
}
