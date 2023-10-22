trait Error: std::error::Error {
    fn span(&self) -> ast::Span;
}
