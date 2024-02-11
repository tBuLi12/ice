use fmt::TextRange;
use ice_parser::Parser;
use iiv::{CursorPosition, Source};
use iiv_gen::{Generator, TokenType};
use lsp_types::{
    request::HoverRequest, Command, CompletionItem, CompletionItemKind, CompletionList,
    CompletionOptions, CompletionParams, CompletionResponse, Diagnostic, DiagnosticOptions,
    DiagnosticServerCapabilities, DiagnosticSeverity, DidChangeTextDocumentParams,
    DidCloseTextDocumentParams, DidOpenTextDocumentParams, DocumentDiagnosticParams,
    DocumentDiagnosticReport, DocumentFormattingParams, FullDocumentDiagnosticReport, Hover,
    HoverContents, HoverParams, HoverProviderCapability, InitializeParams, InitializeResult,
    LanguageString, MarkedString, OneOf, Position, Range, SemanticToken, SemanticTokenModifier,
    SemanticTokenType, SemanticTokens, SemanticTokensFullOptions, SemanticTokensLegend,
    SemanticTokensOptions, SemanticTokensParams, SemanticTokensResult,
    SemanticTokensServerCapabilities, ServerCapabilities, ServerInfo, TextDocumentIdentifier,
    TextDocumentPositionParams, TextDocumentSyncCapability, TextDocumentSyncKind, TextEdit, Url,
    WorkDoneProgressOptions,
};
use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    error::Error,
    io::{self, BufRead, BufReader, Cursor, Read, Seek, SeekFrom, Write},
};

mod fmt;

struct StrSource<'s> {
    name: String,
    text: &'s str,
}

impl<'s> Source for StrSource<'s> {
    type Reader<'this> = Cursor<&'this [u8]>     where
    Self: 'this;

    fn name(&self) -> &str {
        &self.name
    }

    fn reader(&self) -> Self::Reader<'_> {
        Cursor::new(self.text.as_bytes())
    }
}

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
    // if let Err(err) = &res {
    //     let mut f = File::create("C:/Users/tbuli/Apps/DEV/ice/ice-lsp/logs.txt")?;
    //     f.write_all(err.to_string().as_bytes())?;
    // }
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

    let mut files = HashMap::new();

    loop {
        let body = get_body(&mut reader)?;
        do_request(&mut files, body, &mut writer)?;
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
                    text_document_sync: Some(TextDocumentSyncCapability::Kind(
                        TextDocumentSyncKind::FULL,
                    )),
                    // document_formatting_provider: Some(OneOf::Left(true)),
                    document_formatting_provider: None,
                    diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
                        DiagnosticOptions {
                            inter_file_dependencies: false,
                            workspace_diagnostics: false,
                            ..Default::default()
                        },
                    )),
                    completion_provider: Some(CompletionOptions {
                        ..Default::default()
                    }),
                    hover_provider: Some(HoverProviderCapability::Simple(true)),
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

fn do_request(
    files: &mut HashMap<Url, String>,
    body: String,
    writer: &mut impl Write,
) -> Result<(), Box<dyn Error>> {
    let request: Request = serde_json::from_str(&body).map_err(add_ctx("in request body"))?;

    eprintln!("method: {}", request.method);

    let Some(id) = request.id else {
        match request.method {
            "textDocument/didOpen" => {
                let request: Params<DidOpenTextDocumentParams> =
                    serde_json::from_str(&body).map_err(add_ctx("in request params"))?;
                let request = request.params;
                files.insert(request.text_document.uri, request.text_document.text);
            }
            "textDocument/didChange" => {
                let request: Params<DidChangeTextDocumentParams> =
                    serde_json::from_str(&body).map_err(add_ctx("in request params"))?;
                let request = request.params;
                if let Some(text) = files.get_mut(&request.text_document.uri) {
                    *text = request.content_changes.into_iter().next().unwrap().text;
                }
            }
            "textDocument/didClose" => {
                let request: Params<DidCloseTextDocumentParams> =
                    serde_json::from_str(&body).map_err(add_ctx("in request params"))?;
                let request = request.params;
                files.remove(&request.text_document.uri);
            }
            _ => {}
        }

        return Ok(());
    };

    match request.method {
        "textDocument/semanticTokens/full" => {
            let request: Params<SemanticTokensParams> =
                serde_json::from_str(&body).map_err(add_ctx("in request params"))?;
            let request = request.params;

            if let Some(text) = files.get(&request.text_document.uri) {
                let source = source_from_text_document(request.text_document, text);

                let result = SemanticTokensResult::Tokens(SemanticTokens {
                    result_id: None,
                    data: semantic_highlight(&source),
                });

                write_response(result, id, writer)?;
            }
        }
        "textDocument/formatting" => {
            let request: Params<DocumentFormattingParams> = serde_json::from_str(&body)?;
            let request = request.params;
            let edits = if let Some(text) = files.get(&request.text_document.uri) {
                let ctx = iiv::Ctx::new();
                ctx.init();
                let mut parser = Parser::new(&ctx, text.as_bytes());
                let module = parser.parse_program();
                let edits = fmt::format_module(module);
                eprintln!("{:#?}", edits);
                edits
            } else {
                vec![]
            };
            write_response(edits, id, writer)?;
        }
        "textDocument/completion" => {
            let request: Params<CompletionParams> =
                serde_json::from_str(&body).map_err(add_ctx("in request params"))?;
            let position = request.params.text_document_position.position;
            let items = if let Some(text) =
                files.get(&request.params.text_document_position.text_document.uri)
            {
                let source = source_from_text_document(
                    request.params.text_document_position.text_document,
                    text,
                );
                completion(&source, position)
            } else {
                vec![]
            };
            let response = CompletionResponse::List(CompletionList {
                is_incomplete: false,
                items,
            });
            write_response(response, id, writer)?;
        }
        "textDocument/diagnostic" => {
            let request: Params<DocumentDiagnosticParams> =
                serde_json::from_str(&body).map_err(add_ctx("in request params"))?;

            let diagnostics = if let Some(text) = files.get(&request.params.text_document.uri) {
                let source = source_from_text_document(request.params.text_document, text);
                diagnostics(&source)
            } else {
                vec![]
            };
            let response =
                DocumentDiagnosticReport::Full(lsp_types::RelatedFullDocumentDiagnosticReport {
                    related_documents: None,
                    full_document_diagnostic_report: FullDocumentDiagnosticReport {
                        result_id: None,
                        items: diagnostics,
                    },
                });
            write_response(response, id, writer)?;
        }
        "textDocument/hover" => {
            let request: Params<HoverParams> =
                serde_json::from_str(&body).map_err(add_ctx("in request params"))?;

            if let Some(text) = files.get(
                &request
                    .params
                    .text_document_position_params
                    .text_document
                    .uri,
            ) {
                let source = source_from_text_document(
                    request.params.text_document_position_params.text_document,
                    text,
                );
                if let Some(hover) = hover(
                    &source,
                    request.params.text_document_position_params.position,
                ) {
                    let response = Hover {
                        range: None,
                        contents: HoverContents::Scalar(MarkedString::LanguageString(
                            LanguageString {
                                language: "ice".to_string(),
                                value: hover,
                            },
                        )),
                    };
                    write_response(response, id, writer)?;
                } else {
                    write_response(serde_json::json!(null), id, writer)?;
                }
            } else {
                write_response(serde_json::json!(null), id, writer)?;
            }
        }

        "exit" => std::process::exit(0),
        _ => return Err(err("invalid request")),
    }

    Ok(())
}

fn source_from_text_document(document: TextDocumentIdentifier, text: &str) -> StrSource<'_> {
    StrSource {
        name: document
            .uri
            .path_segments()
            .map(|segments| segments.last())
            .flatten()
            .unwrap_or("<unknown file>")
            .to_string(),
        text,
    }
}

fn hover(source: &impl Source, position: Position) -> Option<String> {
    let ctx = iiv::Ctx::new();
    ctx.init();

    let mut parser = Parser::new(&ctx, BufReader::new(source.reader()));
    let mut generator = Generator::new(&ctx, true);

    let module = parser.parse_program();

    let _ = generator.emit_iiv(&[module]);

    generator
        .index
        .0
        .unwrap()
        .into_iter()
        .find_map(|(span, token)| {
            if !span.contains(position.line, position.character) {
                return None;
            }
            match token {
                TokenType::Type { decl } => Some(format!("type {}", decl.name)),
                TokenType::Variable { ty, def_span, .. } => {
                    let mut reader = source.reader();
                    Seek::seek(
                        &mut reader,
                        SeekFrom::Start(
                            (def_span.begin_offset + def_span.begin_highlight_offset) as u64,
                        ),
                    )
                    .unwrap();
                    let mut buf = vec![
                        0u8;
                        (def_span.end_highlight_offset - def_span.begin_highlight_offset)
                            as usize
                    ];
                    reader.read_exact(&mut buf).unwrap();
                    Some(format!(
                        "let {}: {}",
                        String::from_utf8(buf.to_vec()).unwrap(),
                        ty
                    ))
                }
                TokenType::Property { name, ty } => Some(format!("{}: {}", name, ty)),
                TokenType::Function { decl } => Some(format!("fun {}", decl.borrow().sig.name)),
                _ => None,
            }
        })
}

fn completion(source: &impl Source, position: Position) -> Vec<CompletionItem> {
    let ctx = iiv::Ctx::new();
    ctx.init();
    let mut parser = Parser::new(&ctx, BufReader::new(source.reader()));
    let mut generator = Generator::new(&ctx, true);
    parser.set_cursor(position.line, position.character);

    let module = parser.parse_program();
    generator.completion_token = parser.get_completion_token().map(|token| {
        let span = token.span();
        CursorPosition {
            line: span.first_line,
            column: span.begin_highlight_offset,
        }
    });

    let _ = generator.emit_iiv(&[module]);

    dbg!(&generator.completion_items);

    generator
        .completion_items
        .into_iter()
        .map(|item| CompletionItem {
            label: item.text.to_string(),
            kind: Some(match item.ty {
                iiv_gen::CompletionType::Function => CompletionItemKind::FUNCTION,
                iiv_gen::CompletionType::Property => CompletionItemKind::PROPERTY,
                iiv_gen::CompletionType::Variable => CompletionItemKind::VARIABLE,
                iiv_gen::CompletionType::Type => CompletionItemKind::CLASS,
            }),
            ..Default::default()
        })
        .collect()
}

fn diagnostics(source: &impl Source) -> Vec<Diagnostic> {
    let ctx = iiv::Ctx::new();
    ctx.init();
    let mut parser = Parser::new(&ctx, BufReader::new(source.reader()));
    let mut generator = Generator::new(&ctx, true);

    let module = parser.parse_program();

    let _ = generator.emit_iiv(&[module]);

    ctx.diagnostcs
        .take_all()
        .into_iter()
        .map(|diagnostic| Diagnostic {
            range: TextRange::range(diagnostic.span),
            severity: Some(DiagnosticSeverity::ERROR),
            message: diagnostic.message,
            source: Some("ice-lsp".to_string()),
            ..Default::default()
        })
        .collect()
}

fn semantic_highlight(source: &impl Source) -> Vec<SemanticToken> {
    let ctx = iiv::Ctx::new();
    ctx.init();

    let mut parser = Parser::new(&ctx, BufReader::new(source.reader()));
    let mut generator = Generator::new(&ctx, true);

    let module = parser.parse_program();

    let _ = generator.emit_iiv(&[module]);

    if !ctx.diagnostcs.ok() {
        ctx.flush_diagnostics(source);
    }

    let mut prev_line = 0;
    let mut prev_start = 0;

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
                    TokenType::Type { .. } => 0,
                    TokenType::Variable { .. } => 1,
                    TokenType::Property { .. } => 2,
                    TokenType::Function { .. } => 3,
                    TokenType::Keyword { .. } => 4,
                    TokenType::Variant { .. } => 5,
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
