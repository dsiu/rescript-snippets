// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Int from "rescript/lib/es6/Belt_Int.js";
import * as Belt_List from "rescript/lib/es6/Belt_List.js";
import * as Pervasives from "rescript/lib/es6/Pervasives.js";
import * as Belt_Option from "rescript/lib/es6/Belt_Option.js";
import * as Stdlib_List from "rescript/lib/es6/Stdlib_List.js";
import * as Primitive_option from "rescript/lib/es6/Primitive_option.js";

function list_find(_l, f) {
  while (true) {
    let l = _l;
    if (l === 0) {
      return;
    }
    let x = l.hd;
    if (f(x)) {
      return Primitive_option.some(x);
    }
    _l = l.tl;
    continue;
  };
}

console.log(list_find({
  hd: 1,
  tl: {
    hd: 3,
    tl: {
      hd: 5,
      tl: {
        hd: 2,
        tl: /* [] */0
      }
    }
  }
}, x => x > 3));

console.log(list_find({
  hd: 1,
  tl: {
    hd: 3,
    tl: {
      hd: 5,
      tl: {
        hd: 2,
        tl: /* [] */0
      }
    }
  }
}, x => x > 10));

console.log(list_find({
  hd: "a",
  tl: {
    hd: "B",
    tl: {
      hd: "C",
      tl: /* [] */0
    }
  }
}, x => x === x.toUpperCase()));

let If_not_found_1 = {};

function flexible_find_1(_l, f, if_not_found) {
  while (true) {
    let l = _l;
    if (l === 0) {
      if (typeof if_not_found !== "object") {
        if (if_not_found === "Raise") {
          return Pervasives.failwith("Element not found");
        } else {
          return;
        }
      } else {
        return Primitive_option.some(if_not_found._0);
      }
    }
    let hd = l.hd;
    if (f(hd)) {
      return Primitive_option.some(hd);
    }
    _l = l.tl;
    continue;
  };
}

let __x = flexible_find_1({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 5,
      tl: /* [] */0
    }
  }
}, x => x > 10, "Return_none");

console.log("flexible_find_1", __x);

let __x$1 = flexible_find_1({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 5,
      tl: /* [] */0
    }
  }
}, x => x > 10, {
  TAG: "Default_to",
  _0: 10
});

console.log("flexible_find_1", __x$1);

try {
  let __x$2 = flexible_find_1({
    hd: 1,
    tl: {
      hd: 2,
      tl: {
        hd: 5,
        tl: /* [] */0
      }
    }
  }, x => x > 10, "Raise");
  console.log("flexible_find_1", __x$2);
} catch (exn) {
  console.log("flexible_find_1", "nothing > 10");
}

let __x$3 = flexible_find_1({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 20,
      tl: /* [] */0
    }
  }
}, x => x > 10, "Raise");

console.log("flexible_find_1", __x$3);

let If_not_found_1$1 = {
  If_not_found_1: If_not_found_1,
  flexible_find_1: flexible_find_1
};

let If_not_found_2 = {};

function flexible_find_2(f, _list, if_not_found) {
  while (true) {
    let list = _list;
    if (list === 0) {
      if (typeof if_not_found !== "object") {
        if (if_not_found === "Raise") {
          return Pervasives.failwith("No matching item found");
        } else {
          return;
        }
      } else {
        return if_not_found._0;
      }
    }
    let hd = list.hd;
    if (f(hd)) {
      if (typeof if_not_found !== "object" && if_not_found !== "Raise") {
        return Primitive_option.some(hd);
      } else {
        return hd;
      }
    }
    _list = list.tl;
    continue;
  };
}

let __x$4 = flexible_find_2(x => x > 10, {
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 5,
      tl: /* [] */0
    }
  }
}, "Return_none");

console.log("flexible_find_2", __x$4);

let __x$5 = flexible_find_2(x => x > 10, {
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 5,
      tl: /* [] */0
    }
  }
}, {
  TAG: "Default_to",
  _0: 10
});

console.log("flexible_find_2", __x$5);

try {
  let __x$6 = flexible_find_2(x => x > 10, {
    hd: 1,
    tl: {
      hd: 2,
      tl: {
        hd: 5,
        tl: /* [] */0
      }
    }
  }, "Raise");
  console.log("flexible_find_2", __x$6);
} catch (exn$1) {
  console.log("flexible_find_2", "nothing > 10");
}

let __x$7 = flexible_find_2(x => x > 10, {
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 20,
      tl: /* [] */0
    }
  }
}, "Raise");

console.log("flexible_find_2", __x$7);

let If_not_found_2$1 = {
  If_not_found_2: If_not_found_2,
  flexible_find_2: flexible_find_2
};

console.log("Capturing_The_Unknown");

function tuple_i_f(x, y) {
  return [
    x,
    y
  ];
}

function tuple_s_s(x, y) {
  return [
    x,
    y
  ];
}

function print_stringable(s) {
  console.log(s.to_string(s.value));
}

function id(x) {
  return x;
}

let stringables_0 = {
  TAG: "Stringable",
  value: 100,
  to_string: __x => String(__x)
};

let stringables_1 = {
  hd: {
    TAG: "Stringable",
    value: 12.3,
    to_string: __x => String(__x)
  },
  tl: {
    hd: {
      TAG: "Stringable",
      value: "foo",
      to_string: id
    },
    tl: /* [] */0
  }
};

let stringables = {
  hd: stringables_0,
  tl: stringables_1
};

Belt_List.map(stringables, print_stringable);

let Capturing_The_Unknown = {
  tuple_i_f: tuple_i_f,
  tuple_s_s: tuple_s_s,
  print_stringable: print_stringable,
  id: id,
  stringables: stringables
};

function ls_dir(d) {
  switch (d) {
    case "." :
      return {
        hd: "d1",
        tl: {
          hd: "f111",
          tl: {
            hd: "f222",
            tl: {
              hd: "d2",
              tl: {
                hd: "f333",
                tl: /* [] */0
              }
            }
          }
        }
      };
    case ".." :
      return {
        hd: "d10",
        tl: {
          hd: "f100",
          tl: {
            hd: "f200",
            tl: {
              hd: "d2",
              tl: {
                hd: "f300",
                tl: /* [] */0
              }
            }
          }
        }
      };
    default:
      return {
        hd: "d1000",
        tl: {
          hd: "f11100",
          tl: {
            hd: "f22200",
            tl: {
              hd: "d200",
              tl: {
                hd: "f33300",
                tl: /* [] */0
              }
            }
          }
        }
      };
  }
}

function is_file_exn(s) {
  return s[0] === "f";
}

function lstat(s) {
  return {
    st_size: Belt_Option.getWithDefault(Belt_Int.fromString(s.substring(1)), 0)
  };
}

function list_sum(l) {
  return Belt_List.reduce(l, 0, (a, x) => a + x | 0);
}

function sum_file_sizes(d) {
  return list_sum(Belt_List.map(Belt_List.keep(ls_dir(d), is_file_exn), x => lstat(x).st_size));
}

console.log(sum_file_sizes("."));

function add_step(f, pipeline) {
  return {
    TAG: "Step",
    _0: f,
    _1: pipeline
  };
}

function exec(_pipeline, _input) {
  while (true) {
    let input = _input;
    let pipeline = _pipeline;
    if (typeof pipeline !== "object") {
      return input;
    }
    _input = pipeline._0(input);
    _pipeline = pipeline._1;
    continue;
  };
}

let p1_1 = {
  TAG: "Step",
  _0: __x => Belt_List.keep(__x, is_file_exn),
  _1: {
    TAG: "Step",
    _0: __x => Belt_List.map(__x, x => lstat(x).st_size),
    _1: {
      TAG: "Step",
      _0: list_sum,
      _1: "Empty"
    }
  }
};

let p1 = {
  TAG: "Step",
  _0: ls_dir,
  _1: p1_1
};

console.log("using pipeline GADT p1");

console.log(exec(p1, "."));

console.log(exec(p1, ".."));

function exec_with_profile(pipeline, input) {
  let loop = (_pipeline, _input, _rev_profile) => {
    while (true) {
      let rev_profile = _rev_profile;
      let input = _input;
      let pipeline = _pipeline;
      if (typeof pipeline !== "object") {
        return [
          input,
          rev_profile
        ];
      }
      let output = pipeline._0(input);
      _rev_profile = {
        hd: 13,
        tl: rev_profile
      };
      _input = output;
      _pipeline = pipeline._1;
      continue;
    };
  };
  let match = loop(pipeline, input, /* [] */0);
  return [
    match[0],
    Stdlib_List.reverse(match[1])
  ];
}

console.log("using pipeline GADT with profile p1");

console.log(exec_with_profile(p1, "."));

let Abstracting_Computational_Machines = {
  ls_dir: ls_dir,
  is_file_exn: is_file_exn,
  lstat: lstat,
  list_sum: list_sum,
  sum_file_sizes: sum_file_sizes,
  add_step: add_step,
  $plus: add_step,
  empty: "Empty",
  exec: exec,
  p1: p1,
  exec_with_profile: exec_with_profile
};

function get(x) {
  return x._0;
}

let User_name = {};

let User_id = {};

function check(permissions, user_id) {
  return [
    permissions,
    user_id
  ];
}

let Permissions = {
  check: check
};

function set_user_id(request, x) {
  return {
    user_name: request.user_name,
    user_id: {
      TAG: "Present",
      _0: x
    },
    permissions: request.permissions
  };
}

function set_permissions(request, x) {
  return {
    user_name: request.user_name,
    user_id: request.user_id,
    permissions: {
      TAG: "Present",
      _0: x
    }
  };
}

function check_completeness(request) {
  let match = request.user_id;
  let match$1 = request.permissions;
  if (typeof match !== "object" || typeof match$1 !== "object") {
    return;
  } else {
    return {
      user_name: request.user_name,
      user_id: match,
      permissions: match$1
    };
  }
}

function authorized(request) {
  return [
    request.permissions._0,
    request.user_id._0
  ];
}

let Narrowing_the_Possibilities = {
  get: get,
  User_name: User_name,
  User_id: User_id,
  Permissions: Permissions,
  set_user_id: set_user_id,
  set_permissions: set_permissions,
  check_completeness: check_completeness,
  authorized: authorized
};

export {
  list_find,
  If_not_found_1$1 as If_not_found_1,
  If_not_found_2$1 as If_not_found_2,
  Capturing_The_Unknown,
  Abstracting_Computational_Machines,
  Narrowing_the_Possibilities,
}
/*  Not a pure module */
