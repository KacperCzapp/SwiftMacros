// swift-tools-version: 5.9

import PackageDescription
import CompilerPluginSupport

let package = Package(
    name: "SwiftMacros",
    platforms: [.macOS(.v13), .iOS(.v13), .tvOS(.v13), .watchOS(.v9), .macCatalyst(.v13)],
    products: [
        .library(
            name: "SwiftMacros",
            targets: ["SwiftMacros"]
        ),
    ],
    dependencies: [
        .package(url: "https://github.com/apple/swift-syntax.git", from: "509.0.2"),
        .package(url: "https://github.com/apple/swift-docc-plugin", from: "1.1.0"),
        .package(url: "https://github.com/ShenghaiWang/SwiftKeychain.git", from: "0.2.0")
    ],
    targets: [
        .macro(
            name: "Macros",
            dependencies: [
                .product(name: "SwiftSyntaxMacros", package: "swift-syntax"),
                .product(name: "SwiftCompilerPlugin", package: "swift-syntax"),
            ]
        ),
        .target(name: "SwiftMacros", dependencies: ["Macros"]),

        .executableTarget(name: "Client", dependencies: ["SwiftMacros", "SwiftKeychain"]),

        .testTarget(
            name: "MacroTests",
            dependencies: [
                "Macros",
                .product(name: "SwiftSyntaxMacrosTestSupport", package: "swift-syntax"),
            ]
        ),
    ]
)
