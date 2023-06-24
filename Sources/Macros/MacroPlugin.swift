import SwiftCompilerPlugin
import SwiftSyntaxMacros

@main
struct MacroPlugin: CompilerPlugin {
    let providingMacros: [Macro.Type] = [
        AddAssociatedValueVariable.self,
        AddInit.self,
        AddPublisher.self,
        BuildURL.self,
        BuildURLRequest.self,
        Encode.self,
        Decode.self,
        PostNotification.self,
        Singleton.self,
    ]
}
