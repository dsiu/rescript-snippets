// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';


function Collection(Item) {
  return {};
}

var User = {};

var UsersCollection = {};

var Collections = {};

function doSome(firestore) {
  return firestore.collections("users").orderBy("name").asList();
}

var Collection$1 = {};

var User$1 = {};

function doSome$1(firestore) {
  return firestore.collections("users").orderBy("name").asList();
}

var Solution2 = {
  Collection: Collection$1,
  User: User$1,
  doSome: doSome$1
};

exports.Collection = Collection;
exports.User = User;
exports.UsersCollection = UsersCollection;
exports.Collections = Collections;
exports.doSome = doSome;
exports.Solution2 = Solution2;
/* No side effect */
