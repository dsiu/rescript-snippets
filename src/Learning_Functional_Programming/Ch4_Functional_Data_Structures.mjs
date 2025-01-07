// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Stdlib__List from "@dsiu/rescript-stdlib-fp/src/Stdlib__List.mjs";
import * as Stdlib__Result from "@dsiu/rescript-stdlib-fp/src/Stdlib__Result.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function getUser(id) {
  if (id !== 0) {
    return {
      TAG: "Customer",
      _0: id
    };
  }
  
}

let prim = [
  0,
  1,
  2
].flatMap(x => [getUser(x)]);

console.log(prim);

let Ch4_flatmap = {
  getUser: getUser
};

function getUsers() {
  return {
    hd: {
      TAG: "Ok",
      _0: {
        TAG: "User",
        id: 1,
        email: "jack@example.com"
      }
    },
    tl: {
      hd: {
        TAG: "Error",
        _0: {
          TAG: "Error",
          id: 4,
          text: "user not found"
        }
      },
      tl: {
        hd: {
          TAG: "Ok",
          _0: {
            TAG: "User",
            id: 2,
            email: "andrea@example.com"
          }
        },
        tl: /* [] */0
      }
    }
  };
}

let emails = Stdlib__List.map({
  hd: {
    TAG: "Ok",
    _0: {
      TAG: "User",
      id: 1,
      email: "jack@example.com"
    }
  },
  tl: {
    hd: {
      TAG: "Error",
      _0: {
        TAG: "Error",
        id: 4,
        text: "user not found"
      }
    },
    tl: {
      hd: {
        TAG: "Ok",
        _0: {
          TAG: "User",
          id: 2,
          email: "andrea@example.com"
        }
      },
      tl: /* [] */0
    }
  }
}, result => Stdlib__Result.map(result, u => u.email));

let __x = Stdlib__List.toArray(emails);

console.log(36, __x);

let Ch4_Higher_Order_Functions = {
  getUsers: getUsers,
  emails: emails
};

function getStudent(id) {
  return {
    TAG: "Student",
    id: 1,
    email: "alex"
  };
}

function getFinalGrade(student) {
  return {
    TAG: "FinalGrade",
    grade: 100
  };
}

let Ch4_Monads_students = {
  hd: {
    TAG: "Student",
    id: 1,
    email: "jack@example.com"
  },
  tl: {
    hd: undefined,
    tl: {
      hd: {
        TAG: "Student",
        id: 2,
        email: "andrea@example.com"
      },
      tl: {
        hd: undefined,
        tl: /* [] */0
      }
    }
  }
};

let Ch4_Monads = {
  getStudent: getStudent,
  getFinalGrade: getFinalGrade,
  students: Ch4_Monads_students
};

let A;

export {
  A,
  log,
  log2,
  Ch4_flatmap,
  Ch4_Higher_Order_Functions,
  Ch4_Monads,
}
/* prim Not a pure module */
