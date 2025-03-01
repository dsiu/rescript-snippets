// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_List from "rescript/lib/es6/Belt_List.js";
import * as Pervasives from "rescript/lib/es6/Pervasives.js";
import * as Primitive_option from "rescript/lib/es6/Primitive_option.js";

function createEmailAddress(s) {
  let match = /^\S+@\S+\.\S+$/.exec(s);
  if (match !== null) {
    return {
      TAG: "EmailAddress",
      _0: s
    };
  }
  
}

function createStateCode(s) {
  let s$p = s.toUpperCase();
  let match = Belt_List.has({
    hd: "AZ",
    tl: {
      hd: "CA",
      tl: {
        hd: "NY",
        tl: /* [] */0
      }
    }
  }, s$p, (a, b) => a === b);
  if (match) {
    return {
      TAG: "StateCode",
      _0: s$p
    };
  }
  
}

console.log(createStateCode("ca"));

console.log(createStateCode("ba"));

console.log(createEmailAddress("someone@mail.com"));

console.log(createEmailAddress("diudiu"));

function createEmailAddress2(s) {
  let match = /^\S+@\S+\.\S+$/.exec(s);
  if (match !== null) {
    return {
      TAG: "Success",
      _0: {
        TAG: "EmailAddress",
        _0: s
      }
    };
  } else {
    return {
      TAG: "Error",
      _0: "Email address must contain an @ sign"
    };
  }
}

console.log(createEmailAddress2("diudiu"));

function createEmailAddressWithContinuations(success, failure, s) {
  let match = /^\S+@\S+\.\S+$/.exec(s);
  if (match !== null) {
    return success({
      TAG: "EmailAddress",
      _0: s
    });
  } else {
    return failure("Email address must contain an @ sign");
  }
}

function success(e) {
  return Primitive_option.some(e);
}

function failure(param) {
  return Pervasives.failwith("bad email address");
}

console.log(createEmailAddressWithContinuations(success, failure, "someone@mail.com"));

function success$1(e) {
  return Primitive_option.some(e);
}

function failure$1(param) {
  
}

console.log(createEmailAddressWithContinuations(success$1, failure$1, "someone@mail.com"));

function createEmail(__x) {
  return createEmailAddressWithContinuations(success$1, failure$1, __x);
}

console.log(createEmailAddress("someone@mail.com"), "with carry");

console.log(createEmailAddress("diudiu"), "with carry");

function create(s) {
  let match = /^\S+@\S+\.\S+$/.exec(s);
  if (match !== null) {
    return {
      TAG: "EmailAddress",
      _0: s
    };
  }
  
}

function apply(f, e) {
  return f(e._0);
}

function value(e) {
  return e._0;
}

let EmailAddress = {
  create: create,
  apply: apply,
  value: value
};

let address1 = create("x@example.com");

if (address1 !== undefined) {
  console.log(address1._0, "is a valid email address");
}

export {
  createEmailAddress,
  createStateCode,
  createEmailAddress2,
  createEmailAddressWithContinuations,
  success$1 as success,
  failure$1 as failure,
  createEmail,
  EmailAddress,
  address1,
}
/*  Not a pure module */
