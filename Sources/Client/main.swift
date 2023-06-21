import SwiftMacros
import Foundation
import Combine

struct TestSingleton: Encodable {
    let name = "Tim Wang"
    private init() {
    }

    static let shared = Self ()
}

@Singleton
struct TestSingletonMacro {
    let name = "Tim Wang"
}


@Singleton
public struct TestPublicSingletonMacro {
    let name = "Tim Wang"
}

@Singleton
class TestPublicSingletonNSOjbectMacro: NSObject {
    let name = "Tim Wang"
}

print(TestSingleton.shared.name)
print(TestSingletonMacro.shared.name)
print(TestPublicSingletonMacro.shared.name)
print(TestPublicSingletonNSOjbectMacro.shared.name)


struct TestStruct: Codable {
    var name = "Tim Wang"
}

let data = try #encode(TestStruct(), dateEncodingStrategy: .iso8601, dataEncodingStrategy: .base64)
let value = try #decode(TestStruct.self, from: data, dateDecodingStrategy: .deferredToDate)

struct MyType {
    @AddPublisher
    private let mySubject = PassthroughSubject<Void, Never>()
}

_ = MyType().mySubjectPublisher.sink { _ in

}

struct TestNotification {
    func post() {
        #postNotification(.NSCalendarDayChanged, object: NSObject(), userInfo: ["value": 0])
    }
}

@AddInit
struct InitStruct {
    let a: Int
    let b: Int?
    let c: (Int?) -> Void
    let d: ((Int?) -> Void)?
}
