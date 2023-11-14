import SwiftCompilerPlugin
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

public struct Encode: ExpressionMacro {
    public static func expansion<Node: FreestandingMacroExpansionSyntax,
                          Context: MacroExpansionContext>(of node: Node,
                                                          in context: Context) throws -> ExprSyntax {
        let defaults = [
            "outputFormatting": "[]",
            "dateEncodingStrategy": "deferredToDate",
            "dataEncodingStrategy": "deferredToData",
            "nonConformingFloatEncodingStrategy": "throw",
            "keyEncodingStrategy": "useDefaultKeys",
            "userInfo": "[:]"]

        guard let value = node.argumentList.first(where: { $0.label == nil })?.expression else {
            throw MacroDiagnostics.errorMacroUsage(message: "Must specify the value to encode")
        }
        let encoder: DeclSyntax = "let encoder = JSONEncoder()"
        let encoderStatement = CodeBlockItemSyntax(item: .decl(encoder), trailingTrivia: .newline)
        let mappedArgsList = node.argumentList
            .filter { $0.label != nil }
            .compactMap { tupleExprElementSyntax in
            if let parameter = tupleExprElementSyntax.label?.text,
               defaults[parameter] != "\(tupleExprElementSyntax.expression)" {
                let stmt: StmtSyntax = "encoder.\(raw: parameter) = \(tupleExprElementSyntax.expression)"
                return CodeBlockItemSyntax(item: .stmt(stmt), trailingTrivia: .newline)
            }
            return nil
        }

        let returnValue: ExprSyntax = "return try encoder.encode(\(value))"
        let returnblock = CodeBlockItemSyntax(item: .expr(returnValue), trailingTrivia: .newline)

        let resultCodeBlock = CodeBlockItemListSyntax {
            encoderStatement
            CodeBlockItemListSyntax(mappedArgsList)
            returnblock
        }
        let closure = ClosureExprSyntax(statements: resultCodeBlock)
        let function = FunctionCallExprSyntax(callee: closure)
        return ExprSyntax(function)
    }
}

public struct Decode: ExpressionMacro {
    public static func expansion<Node: FreestandingMacroExpansionSyntax,
                                 Context: MacroExpansionContext>(of node: Node,
                                                                 in context: Context) throws -> ExprSyntax {
        let defaults = [
            "dateDecodingStrategy": "deferredToDate",
            "dataDecodingStrategy": "deferredToData",
            "nonConformingFloatDecodingStrategy": "throw",
            "keyDecodingStrategy": "useDefaultKeys",
            "userInfo": "[:]",
            "allowsJSON5": "false",
            "assumesTopLevelDictionary": "false"]
        guard let type = node.argumentList.first(where: { $0.label == nil })?.expression,
              let data = node.argumentList.first(where: { $0.label?.text == "from" })?.expression else {
            throw MacroDiagnostics.errorMacroUsage(message: "Must specify the type and the value to decode")
        }
        let decoder: DeclSyntax = "let decoder = JSONDecoder()"
        let decoderStatement = CodeBlockItemSyntax(item: .decl(decoder), trailingTrivia: .newline)

        let mappedArgsList = node.argumentList
            .filter { $0.label != nil && $0.label?.text != "from" }
            .compactMap { tupleExprElementSyntax in
            if let parameter = tupleExprElementSyntax.label?.text,
               defaults[parameter] != "\(tupleExprElementSyntax.expression)" {
                let stmt: StmtSyntax = "decoder.\(raw: tupleExprElementSyntax.label?.text ?? "") = \(tupleExprElementSyntax.expression)"
                return CodeBlockItemSyntax(item: .stmt(stmt), trailingTrivia: .newline)
            }
            return nil
        }

        let returnValue: ExprSyntax = "return try decoder.decode(\(type), from: \(data))"
        let returnblock = CodeBlockItemSyntax(item: .expr(returnValue), trailingTrivia: .newline)

        let resultCodeBlock = CodeBlockItemListSyntax {
            decoderStatement
            CodeBlockItemListSyntax(mappedArgsList)
            returnblock
        }
        let closure = ClosureExprSyntax(statements: resultCodeBlock)
        let function = FunctionCallExprSyntax(callee: closure)
        return ExprSyntax(function)
    }
}
