// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Test from "rescript-test/src/Test.mjs";
import * as Lists_99 from "./Lists_99.mjs";
import * as Belt_List from "rescript/lib/es6/Belt_List.js";
import * as Test_Utils from "../../interop/Test_Utils.mjs";

Test.test("1. last", () => {
  Test_Utils.optionEqual(undefined, Lists_99.last({
    hd: "a",
    tl: {
      hd: "b",
      tl: {
        hd: "c",
        tl: {
          hd: "d",
          tl: /* [] */0
        }
      }
    }
  }), "d");
  Test_Utils.optionEqual(undefined, Lists_99.last(/* [] */0), undefined);
});

Test.test("2. last_two", () => {
  Test_Utils.optionListEqual(undefined, Lists_99.last_two({
    hd: "a",
    tl: {
      hd: "b",
      tl: {
        hd: "c",
        tl: {
          hd: "d",
          tl: /* [] */0
        }
      }
    }
  }), {
    hd: "c",
    tl: {
      hd: "d",
      tl: /* [] */0
    }
  });
  Test_Utils.optionListEqual(undefined, Lists_99.last_two({
    hd: "a",
    tl: /* [] */0
  }), undefined);
});

Test.test("3. at", () => {
  Test_Utils.optionEqual(undefined, Lists_99.at({
    hd: "a",
    tl: {
      hd: "b",
      tl: {
        hd: "c",
        tl: {
          hd: "d",
          tl: {
            hd: "e",
            tl: /* [] */0
          }
        }
      }
    }
  }, 3), "c");
  Test_Utils.optionEqual(undefined, Lists_99.at({
    hd: "a",
    tl: /* [] */0
  }, 3), undefined);
});

Test.test("4. length", () => {
  Test_Utils.intEqual(undefined, Lists_99.length({
    hd: "a",
    tl: {
      hd: "b",
      tl: {
        hd: "c",
        tl: {
          hd: "d",
          tl: {
            hd: "e",
            tl: /* [] */0
          }
        }
      }
    }
  }), 5);
  Test_Utils.intEqual(undefined, Lists_99.length({
    hd: "a",
    tl: /* [] */0
  }), 1);
  Test_Utils.intEqual(undefined, Lists_99.length(/* [] */0), 0);
});

Test.test("5. rev", () => {
  Test_Utils.listEqual(undefined, Lists_99.rev({
    hd: "a",
    tl: {
      hd: "b",
      tl: {
        hd: "c",
        tl: {
          hd: "d",
          tl: {
            hd: "e",
            tl: /* [] */0
          }
        }
      }
    }
  }), {
    hd: "e",
    tl: {
      hd: "d",
      tl: {
        hd: "c",
        tl: {
          hd: "b",
          tl: {
            hd: "a",
            tl: /* [] */0
          }
        }
      }
    }
  });
  Test_Utils.listEqual(undefined, Lists_99.rev(/* [] */0), /* [] */0);
});

Test.test("6. is_palindrome", () => {
  Test_Utils.boolEqual(undefined, Lists_99.is_palindrome({
    hd: "x",
    tl: {
      hd: "a",
      tl: {
        hd: "m",
        tl: {
          hd: "a",
          tl: {
            hd: "x",
            tl: /* [] */0
          }
        }
      }
    }
  }), true);
  Test_Utils.boolEqual(undefined, Lists_99.is_palindrome({
    hd: "a",
    tl: {
      hd: "b",
      tl: {
        hd: "c",
        tl: /* [] */0
      }
    }
  }), false);
  Test_Utils.boolEqual(undefined, Lists_99.is_palindrome({
    hd: "a",
    tl: /* [] */0
  }), true);
  Test_Utils.boolEqual(undefined, Lists_99.is_palindrome(/* [] */0), true);
});

Test.test("7. flatten", () => Test_Utils.listEqual(undefined, Lists_99.flatten({
  hd: {
    TAG: "One",
    _0: "a"
  },
  tl: {
    hd: {
      TAG: "Many",
      _0: {
        hd: {
          TAG: "One",
          _0: "b"
        },
        tl: {
          hd: {
            TAG: "Many",
            _0: {
              hd: {
                TAG: "One",
                _0: "c"
              },
              tl: {
                hd: {
                  TAG: "One",
                  _0: "d"
                },
                tl: /* [] */0
              }
            }
          },
          tl: {
            hd: {
              TAG: "One",
              _0: "e"
            },
            tl: /* [] */0
          }
        }
      }
    },
    tl: /* [] */0
  }
}), {
  hd: "a",
  tl: {
    hd: "b",
    tl: {
      hd: "c",
      tl: {
        hd: "d",
        tl: {
          hd: "e",
          tl: /* [] */0
        }
      }
    }
  }
}));

Test.test("8. compress", () => {
  Test_Utils.listEqual(undefined, Lists_99.compress({
    hd: "a",
    tl: {
      hd: "a",
      tl: {
        hd: "a",
        tl: {
          hd: "a",
          tl: {
            hd: "b",
            tl: {
              hd: "c",
              tl: {
                hd: "c",
                tl: {
                  hd: "a",
                  tl: {
                    hd: "a",
                    tl: {
                      hd: "d",
                      tl: {
                        hd: "e",
                        tl: {
                          hd: "e",
                          tl: {
                            hd: "e",
                            tl: {
                              hd: "e",
                              tl: /* [] */0
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }), {
    hd: "a",
    tl: {
      hd: "b",
      tl: {
        hd: "c",
        tl: {
          hd: "a",
          tl: {
            hd: "d",
            tl: {
              hd: "e",
              tl: /* [] */0
            }
          }
        }
      }
    }
  });
  Test_Utils.listEqual(undefined, Lists_99.compress({
    hd: "a",
    tl: {
      hd: "a",
      tl: /* [] */0
    }
  }), {
    hd: "a",
    tl: /* [] */0
  });
  Test_Utils.listEqual(undefined, Lists_99.compress({
    hd: "a",
    tl: /* [] */0
  }), {
    hd: "a",
    tl: /* [] */0
  });
});

Test.test("9. pack", () => {
  Belt_List.toArray(Belt_List.map(Lists_99.pack({
    hd: "a",
    tl: {
      hd: "a",
      tl: {
        hd: "a",
        tl: {
          hd: "a",
          tl: {
            hd: "b",
            tl: {
              hd: "c",
              tl: {
                hd: "c",
                tl: {
                  hd: "a",
                  tl: {
                    hd: "a",
                    tl: {
                      hd: "d",
                      tl: {
                        hd: "d",
                        tl: {
                          hd: "e",
                          tl: {
                            hd: "e",
                            tl: {
                              hd: "e",
                              tl: {
                                hd: "e",
                                tl: /* [] */0
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }), Belt_List.toArray));
});

Test.test("10. encode", () => {
  Belt_List.toArray(Lists_99.encode({
    hd: "a",
    tl: {
      hd: "a",
      tl: {
        hd: "a",
        tl: {
          hd: "a",
          tl: {
            hd: "b",
            tl: {
              hd: "c",
              tl: {
                hd: "c",
                tl: {
                  hd: "a",
                  tl: {
                    hd: "a",
                    tl: {
                      hd: "d",
                      tl: {
                        hd: "e",
                        tl: {
                          hd: "e",
                          tl: {
                            hd: "e",
                            tl: {
                              hd: "e",
                              tl: /* [] */0
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }));
});

Test.test("11. encode'", () => {
  let __x = Belt_List.toArray(Lists_99.encode_11({
    hd: "a",
    tl: {
      hd: "a",
      tl: {
        hd: "a",
        tl: {
          hd: "a",
          tl: {
            hd: "b",
            tl: {
              hd: "c",
              tl: {
                hd: "c",
                tl: {
                  hd: "a",
                  tl: {
                    hd: "a",
                    tl: {
                      hd: "d",
                      tl: {
                        hd: "e",
                        tl: {
                          hd: "e",
                          tl: {
                            hd: "e",
                            tl: {
                              hd: "e",
                              tl: /* [] */0
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }));
  console.log("11. encode result", __x);
  let __x$1 = [
    {
      TAG: "Many",
      _0: 4,
      _1: "a"
    },
    {
      TAG: "One",
      _0: "b"
    },
    {
      TAG: "Many",
      _0: 2,
      _1: "c"
    },
    {
      TAG: "Many",
      _0: 2,
      _1: "a"
    },
    {
      TAG: "One",
      _0: "d"
    },
    {
      TAG: "Many",
      _0: 4,
      _1: "e"
    }
  ];
  console.log("11. encode expected", __x$1);
});

Test.test("12. decode", () => {
  let __x = Belt_List.toArray(Lists_99.decode({
    hd: {
      TAG: "Many",
      _0: 4,
      _1: "a"
    },
    tl: {
      hd: {
        TAG: "One",
        _0: "b"
      },
      tl: {
        hd: {
          TAG: "Many",
          _0: 2,
          _1: "c"
        },
        tl: {
          hd: {
            TAG: "Many",
            _0: 2,
            _1: "a"
          },
          tl: {
            hd: {
              TAG: "One",
              _0: "d"
            },
            tl: {
              hd: {
                TAG: "Many",
                _0: 4,
                _1: "e"
              },
              tl: /* [] */0
            }
          }
        }
      }
    }
  }));
  console.log("12. decode result", __x);
  let __x$1 = Belt_List.toArray({
    hd: "a",
    tl: {
      hd: "a",
      tl: {
        hd: "a",
        tl: {
          hd: "a",
          tl: {
            hd: "b",
            tl: {
              hd: "c",
              tl: {
                hd: "c",
                tl: {
                  hd: "a",
                  tl: {
                    hd: "a",
                    tl: {
                      hd: "d",
                      tl: {
                        hd: "e",
                        tl: {
                          hd: "e",
                          tl: {
                            hd: "e",
                            tl: {
                              hd: "e",
                              tl: /* [] */0
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  });
  console.log("12. decode expected", __x$1);
});

Test.test("13. encode", () => {
  let __x = Belt_List.toArray(Lists_99.encode_13({
    hd: "a",
    tl: {
      hd: "a",
      tl: {
        hd: "a",
        tl: {
          hd: "a",
          tl: {
            hd: "b",
            tl: {
              hd: "c",
              tl: {
                hd: "c",
                tl: {
                  hd: "a",
                  tl: {
                    hd: "a",
                    tl: {
                      hd: "d",
                      tl: {
                        hd: "e",
                        tl: {
                          hd: "e",
                          tl: {
                            hd: "e",
                            tl: {
                              hd: "e",
                              tl: /* [] */0
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }));
  console.log("13. encode result", __x);
  let __x$1 = [
    {
      TAG: "Many",
      _0: 4,
      _1: "a"
    },
    {
      TAG: "One",
      _0: "b"
    },
    {
      TAG: "Many",
      _0: 2,
      _1: "c"
    },
    {
      TAG: "Many",
      _0: 2,
      _1: "a"
    },
    {
      TAG: "One",
      _0: "d"
    },
    {
      TAG: "Many",
      _0: 4,
      _1: "e"
    }
  ];
  console.log("13. encode expected", __x$1);
});

Test.test("14. duplicate", () => {
  let result = Lists_99.duplicate({
    hd: "a",
    tl: {
      hd: "b",
      tl: {
        hd: "c",
        tl: {
          hd: "c",
          tl: {
            hd: "d",
            tl: /* [] */0
          }
        }
      }
    }
  });
  Test_Utils.listEqual(undefined, result, {
    hd: "a",
    tl: {
      hd: "a",
      tl: {
        hd: "b",
        tl: {
          hd: "b",
          tl: {
            hd: "c",
            tl: {
              hd: "c",
              tl: {
                hd: "c",
                tl: {
                  hd: "c",
                  tl: {
                    hd: "d",
                    tl: {
                      hd: "d",
                      tl: /* [] */0
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  });
});

Test.test("15. replicate", () => {
  let result = Lists_99.replicate({
    hd: "a",
    tl: {
      hd: "b",
      tl: {
        hd: "c",
        tl: /* [] */0
      }
    }
  }, 3);
  Test_Utils.listEqual(undefined, result, {
    hd: "a",
    tl: {
      hd: "a",
      tl: {
        hd: "a",
        tl: {
          hd: "b",
          tl: {
            hd: "b",
            tl: {
              hd: "b",
              tl: {
                hd: "c",
                tl: {
                  hd: "c",
                  tl: {
                    hd: "c",
                    tl: /* [] */0
                  }
                }
              }
            }
          }
        }
      }
    }
  });
});

Test.test("16. drop", () => {
  let result = Lists_99.drop({
    hd: "a",
    tl: {
      hd: "b",
      tl: {
        hd: "c",
        tl: {
          hd: "d",
          tl: {
            hd: "e",
            tl: {
              hd: "f",
              tl: {
                hd: "g",
                tl: {
                  hd: "h",
                  tl: {
                    hd: "i",
                    tl: {
                      hd: "j",
                      tl: /* [] */0
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }, 3);
  Test_Utils.listEqual(undefined, result, {
    hd: "a",
    tl: {
      hd: "b",
      tl: {
        hd: "d",
        tl: {
          hd: "e",
          tl: {
            hd: "g",
            tl: {
              hd: "h",
              tl: {
                hd: "j",
                tl: /* [] */0
              }
            }
          }
        }
      }
    }
  });
});

Test.test("17. split", () => {
  let match = Lists_99.split({
    hd: "a",
    tl: {
      hd: "b",
      tl: {
        hd: "c",
        tl: {
          hd: "d",
          tl: {
            hd: "e",
            tl: {
              hd: "f",
              tl: {
                hd: "g",
                tl: {
                  hd: "h",
                  tl: {
                    hd: "i",
                    tl: {
                      hd: "j",
                      tl: /* [] */0
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }, 3);
  Test_Utils.listEqual(undefined, match[0], {
    hd: "a",
    tl: {
      hd: "b",
      tl: {
        hd: "c",
        tl: /* [] */0
      }
    }
  });
  Test_Utils.listEqual(undefined, match[1], {
    hd: "d",
    tl: {
      hd: "e",
      tl: {
        hd: "f",
        tl: {
          hd: "g",
          tl: {
            hd: "h",
            tl: {
              hd: "i",
              tl: {
                hd: "j",
                tl: /* [] */0
              }
            }
          }
        }
      }
    }
  });
});

Test.test("18. slice", () => {
  let result = Lists_99.slice({
    hd: "a",
    tl: {
      hd: "b",
      tl: {
        hd: "c",
        tl: {
          hd: "d",
          tl: {
            hd: "e",
            tl: {
              hd: "f",
              tl: {
                hd: "g",
                tl: {
                  hd: "h",
                  tl: {
                    hd: "i",
                    tl: {
                      hd: "j",
                      tl: /* [] */0
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }, 2, 6);
  Test_Utils.listEqual(undefined, result, {
    hd: "c",
    tl: {
      hd: "d",
      tl: {
        hd: "e",
        tl: {
          hd: "f",
          tl: {
            hd: "g",
            tl: /* [] */0
          }
        }
      }
    }
  });
});

Test.test("19. rotate", () => {
  let result = Lists_99.rotate({
    hd: "a",
    tl: {
      hd: "b",
      tl: {
        hd: "c",
        tl: {
          hd: "d",
          tl: {
            hd: "e",
            tl: {
              hd: "f",
              tl: {
                hd: "g",
                tl: {
                  hd: "h",
                  tl: /* [] */0
                }
              }
            }
          }
        }
      }
    }
  }, 3);
  Test_Utils.listEqual(undefined, result, {
    hd: "d",
    tl: {
      hd: "e",
      tl: {
        hd: "f",
        tl: {
          hd: "g",
          tl: {
            hd: "h",
            tl: {
              hd: "a",
              tl: {
                hd: "b",
                tl: {
                  hd: "c",
                  tl: /* [] */0
                }
              }
            }
          }
        }
      }
    }
  });
});

Test.test("20. remove_at", () => {
  let result = Lists_99.remove_at({
    hd: "a",
    tl: {
      hd: "b",
      tl: {
        hd: "c",
        tl: {
          hd: "d",
          tl: /* [] */0
        }
      }
    }
  }, 1);
  Test_Utils.listEqual(undefined, result, {
    hd: "a",
    tl: {
      hd: "c",
      tl: {
        hd: "d",
        tl: /* [] */0
      }
    }
  });
});

Test.test("21. insert_at", () => {
  let result = Belt_List.toArray(Lists_99.insert_at({
    hd: "a",
    tl: {
      hd: "b",
      tl: {
        hd: "c",
        tl: {
          hd: "d",
          tl: /* [] */0
        }
      }
    }
  }, 1, "alfa"));
  let expected = Belt_List.toArray({
    hd: "a",
    tl: {
      hd: "alfa",
      tl: {
        hd: "b",
        tl: {
          hd: "c",
          tl: {
            hd: "d",
            tl: /* [] */0
          }
        }
      }
    }
  });
  Test_Utils.stringArrayEqual(undefined, result, expected);
});

Test.test("22. range", () => {
  let result = Belt_List.toArray(Lists_99.range(4, 9));
  let expected = Belt_List.toArray({
    hd: 4,
    tl: {
      hd: 5,
      tl: {
        hd: 6,
        tl: {
          hd: 7,
          tl: {
            hd: 8,
            tl: {
              hd: 9,
              tl: /* [] */0
            }
          }
        }
      }
    }
  });
  Test_Utils.intArrayEqual(undefined, result, expected);
});

Test.test("23. range", () => {
  let result = Belt_List.toArray(Lists_99.range_tail_recur(3, 11));
  let expected = Belt_List.toArray(Lists_99.range(3, 11));
  Test_Utils.intArrayEqual(undefined, result, expected);
});

Test.test("24. rand_select", () => {
  let result = Belt_List.toArray(Lists_99.rand_select({
    hd: "a",
    tl: {
      hd: "b",
      tl: {
        hd: "c",
        tl: {
          hd: "d",
          tl: {
            hd: "e",
            tl: {
              hd: "f",
              tl: {
                hd: "g",
                tl: {
                  hd: "h",
                  tl: {
                    hd: "i",
                    tl: {
                      hd: "j",
                      tl: /* [] */0
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }, 3));
  let expected = Belt_List.toArray({
    hd: "d",
    tl: {
      hd: "i",
      tl: {
        hd: "e",
        tl: /* [] */0
      }
    }
  });
  Test_Utils.intArrayEqual(undefined, result, expected);
});

/*  Not a pure module */
