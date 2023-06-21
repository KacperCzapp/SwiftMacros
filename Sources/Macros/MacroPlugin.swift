import SwiftCompilerPlugin
import SwiftSyntaxMacros

@main
struct MacroPlugin: CompilerPlugin {
    let providingMacros: [Macro.Type] = [
        AddInit.self,
        AddPublisher.self,
        Encode.self,
        Decode.self,
        PostNotification.self,
        Singleton.self,
    ]
}
