module type Ring = {
  type t

  let zero: t

  let one: t

  let \"+": (t, t) => t

  let \"~-": t => t

  let \"*": (t, t) => t

  let to_string: t => string

  let of_int: int => t
}

module type Field = {
  type t

  let zero: t

  let one: t

  let \"+": (t, t) => t

  let \"~-": t => t

  let \"*": (t, t) => t

  let \"/": (t, t) => t

  let to_string: t => string

  let of_int: int => t
}

module IntRing: Ring = {
  type t = int

  let zero = 0

  let one = 1

  let \"+" = \"+"

  let \"~-" = \"~-"

  let \"*" = \"*"

  let to_string = string_of_int

  let of_int = n => n
}

module IntField: Field = {
  type t = int

  let zero = 0

  let one = 1

  let \"+" = \"+"

  let \"~-" = \"~-"

  let \"*" = \"*"

  let \"/" = \"/"

  let to_string = string_of_int

  let of_int = n => n
}

module FloatRing: Ring = {
  type t = float

  let zero = 0.

  let one = 1.

  let \"+" = \"+."

  let \"~-" = \"~-."

  let \"*" = \"*."

  let to_string = Float.toString(_)

  let of_int = n => float_of_int(n)
}

module FloatField: Field = {
  type t = float

  let zero = 0.

  let one = 1.

  let \"+" = \"+."

  let \"~-" = \"~-."

  let \"*" = \"*."

  let \"/" = \"/."

  let to_string = Float.toString(_)

  let of_int = n => float_of_int(n)
}

module IntRational: Field = {
  type t = (int, int)

  let zero = (0, 0)

  let one = (1, 1)

  let \"+" = ((a, b), (c, d)) => (a * d + c * b, b * d)

  let \"~-" = ((a, b)) => (-a, b)

  let \"/" = ((a, b), (c, d)) => (a * d, b * c)

  let \"*" = ((a, b), (c, d)) => (a * c, b * d)

  let to_string = ((a, b)) => string_of_int(a) ++ "/" ++ string_of_int(b)

  let of_int = n => (n, 1)
}

module FloatRational: Field = {
  type t = (float, float)

  let zero = (0., 0.)

  let one = (1., 1.)

  let \"+" = ((a, b), (c, d)) => (a *. d +. c *. b, b *. d)

  let \"~-" = ((a, b)) => (-.a, b)

  let \"/" = ((a, b), (c, d)) => (a *. d, b *. c)

  let \"*" = ((a, b), (c, d)) => (a *. c, b *. d)

  let to_string = ((a, b)) => Float.toString(a) ++ "/" ++ Float.toString(b)

  let of_int = n => (float_of_int(n), 1.)
}
