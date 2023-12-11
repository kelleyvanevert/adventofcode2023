use adventlang::runtime::execute_simple;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

#[wasm_bindgen]
pub fn greet(name: &str) {
    log(&format!("Hello, {}!", name));
}

#[wasm_bindgen(js_name = "executeAdventCode")]
pub fn execute_advent_code(code: &str) -> JsValue {
    execute_simple(code)
        .ok()
        .map(|value| serde_wasm_bindgen::to_value(&value).unwrap())
        .unwrap_or(JsValue::FALSE)
}
