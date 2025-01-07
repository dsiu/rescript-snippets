type t<'a>
include module type of Fqueue with type t<'a> := t<'a>
include Foldable.Extension with type t<'a> := t<'a>
