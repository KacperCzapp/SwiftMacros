import SwiftCompilerPlugin
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

public struct FormatDateComponents: ExpressionMacro {
    public static func expansion<Node: FreestandingMacroExpansionSyntax,
                                 Context: MacroExpansionContext>(of node: Node,
                                                                 in context: Context) throws -> ExprSyntax {
        let statementList: CodeBlockItemListSyntax
        if let fromDate = node.argument(for: "from"),
           let toDate = node.argument(for: "to") {
            statementList = parseFromToFunction(for: node.argumentList, from: fromDate, to: toDate)
        } else if let fromInterval = node.argument(for: "fromInterval") {
            statementList = parseFromFunction(for: node.argumentList, from: fromInterval)
        } else if let fromComponents = node.argument(for: "fromComponents") {
            statementList = parseFromFunction(for: node.argumentList, from: fromComponents)
        } else {
            throw MacroDiagnostics.errorMacroUsage(message: "Not supported parameters")
        }
        let closure = ClosureExprSyntax(statements: statementList)
        let function = FunctionCallExprSyntax(callee: closure)
        return ExprSyntax(function)
    }

    private static func parseFromToFunction(for argumentList: LabeledExprListSyntax,
                                            from fromDate: ExprSyntax,
                                            to toDate: ExprSyntax) -> CodeBlockItemListSyntax {
        let formatter: DeclSyntax = "let formatter = DateComponentsFormatter()"
        let formatterStatement = CodeBlockItemSyntax(item: .decl(formatter), trailingTrivia: .newline)
        let statementList = argumentList.dropFirst(2).compactMap(codeblock)
        let returnValue: ExprSyntax = "return formatter.string(from: \(fromDate), to: \(toDate))"
        let returnblock = CodeBlockItemSyntax(leadingTrivia: .spaces(4),
                                              item: .expr(returnValue),
                                              trailingTrivia: .newline)
        return CodeBlockItemListSyntax {
            formatterStatement
            CodeBlockItemListSyntax(statementList)
            returnblock
        }
    }

    private static func parseFromFunction(for argumentList: LabeledExprListSyntax,
                                          from fromInterval: ExprSyntax) -> CodeBlockItemListSyntax {
        let formatter: DeclSyntax = "let formatter = DateComponentsFormatter()"
        let formatterStatement = CodeBlockItemSyntax(item: .decl(formatter), trailingTrivia: .newline)
        let statementList = argumentList.dropFirst(1).compactMap(codeblock)
        let returnValue: ExprSyntax = "return formatter.string(from: \(fromInterval))"
        let returnblock = CodeBlockItemSyntax(leadingTrivia: .spaces(4),
                                              item: .expr(returnValue),
                                              trailingTrivia: .newline)

        return CodeBlockItemListSyntax {
            formatterStatement
            CodeBlockItemListSyntax(statementList)
            returnblock
        }
    }

    private static func codeblock(for tupleExprElementSyntax: LabeledExprSyntax) -> CodeBlockItemSyntax? {
        if let parameter = tupleExprElementSyntax.label?.text,
           !tupleExprElementSyntax.expression.is(NilLiteralExprSyntax.self) {
            let stmt: StmtSyntax = "formatter.\(raw: parameter) = \(tupleExprElementSyntax.expression)"
            let codeblock = CodeBlockItemSyntax(item: .stmt(stmt), trailingTrivia: .newline)
            return codeblock
        }
        return nil
    }
}
