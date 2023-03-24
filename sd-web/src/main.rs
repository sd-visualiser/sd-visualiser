use dioxus::prelude::*;

fn app(cx: Scope) -> Element {
    let mut count = use_state(&cx, || 0);

    cx.render(rsx!(
        h1 { "Hello {count}" }
        button { onclick: move |_| count += 1, "+" }
        button { onclick: move |_| count -= 1, "-" }
    ))
}

fn main() {
    dioxus_desktop::launch(app)
}
