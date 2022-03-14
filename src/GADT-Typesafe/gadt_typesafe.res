//
// ref: https://shulhi.com/2022/03/09/approaches-to-type-safe-js-interop-design/
//

type metadata = {
  src: string,
  mask: string,
  maskAngle: float,
}

let default = {
  src: "/static/images/image-placeholder.jpg",
  mask: "/static/images/editor/stroke-01.svg",
  maskAngle: 0.,
}

type rec property<'a> =
  | Src: property<string>
  | Mask: property<string>
  | MaskAngle: property<float>

type rec someProperty = SomeProperty(property<'a>): someProperty

let toString = (prop: someProperty) => {
  switch prop {
  | SomeProperty(Src) => "src"
  | SomeProperty(Mask) => "mask"
  | SomeProperty(MaskAngle) => "maskAngle"
  }
}

type t
@get @return(nullable) external metadata: t => option<metadata> = "metadata"
@set external setMetadata: (t, metadata) => unit = "metadata"

@get_index @return(nullable) external js_getProperty: (metadata, string) => option<'a> = ""

let getProperty = (metadata: metadata, prop: property<'a>): 'a => {
  let someProp = SomeProperty(prop)->toString

  switch js_getProperty(metadata, someProp) {
  | None =>
    // this is safe because we know `default` has the property
    js_getProperty(default, someProp)->Belt.Option.getUnsafe
  | Some(v) => v
  }
}

let _ = getProperty(default, Src)->Js.log2("src")
let _ = getProperty(default, MaskAngle)->Js.log2("maskAngle")
