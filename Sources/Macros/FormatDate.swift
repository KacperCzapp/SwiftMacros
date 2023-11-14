import SwiftCompilerPlugin
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

public struct FormatDate: ExpressionMacro {
    public static func expansion<Node: FreestandingMacroExpansionSyntax,
                                 Context: MacroExpansionContext>(of node: Node,
                                                                 in context: Context) throws -> ExprSyntax {
        guard let date = node.argumentList.first else {
            throw MacroDiagnostics.errorMacroUsage(message: "Must specify arguments")
        }

        let formatter: DeclSyntax = "let formatter = DateFormatter()"
        let formatterStatement = CodeBlockItemSyntax(item: .decl(formatter), trailingTrivia: .newline)
        let statementList = node.argumentList
            .filter { $0.label != nil }
            .compactMap { tupleExprElementSyntax in
                if let parameter = tupleExprElementSyntax.label?.text,
                   !tupleExprElementSyntax.expression.is(NilLiteralExprSyntax.self) {
                    let stmt: StmtSyntax = "formatter.\(raw: parameter) = \(tupleExprElementSyntax.expression)"
                    return CodeBlockItemSyntax(item: .stmt(stmt), trailingTrivia: .newline)
                }
                return nil
            }
        let returnValue: ExprSyntax = "return formatter.string(from: \(date.expression))"
        let returnblock = CodeBlockItemSyntax(item: .expr(returnValue), trailingTrivia: .newline)
        let codeblock = CodeBlockItemListSyntax {
            formatterStatement
            CodeBlockItemListSyntax(statementList)
            returnblock
        }
        let closure = ClosureExprSyntax(statements: codeblock)
        let function = FunctionCallExprSyntax(callee: closure)
        return ExprSyntax(function)
    }
}
