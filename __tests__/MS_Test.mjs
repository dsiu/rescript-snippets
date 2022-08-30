// Generated by ReScript, PLEASE EDIT WITH CARE

import * as MS from "../src/MS.mjs";
import Ms from "ms";
import * as Jest2 from "../interop/Jest2.mjs";

describe("MS", (function () {
        var fromStr_Tests = [
          [
            Ms("10h"),
            36000000
          ],
          [
            Ms("2.5 hrs"),
            9000000
          ],
          [
            Ms("2h"),
            7200000
          ],
          [
            Ms("1m"),
            60000
          ],
          [
            Ms("5s"),
            5000
          ],
          [
            Ms("1y"),
            31557600000
          ],
          [
            Ms("100"),
            100
          ],
          [
            Ms("-3 days"),
            -259200000
          ],
          [
            Ms("-1h"),
            -3600000
          ],
          [
            Ms("-200"),
            -200
          ]
        ];
        Jest2.testEach2("fromStr", fromStr_Tests, (function (result, expected) {
                expect(result).toEqual(expected);
              }));
        var fromMS_Tests = [
          [
            Ms(60000),
            "1m"
          ],
          [
            Ms(2 * 60000),
            "2m"
          ],
          [
            Ms(-3 * 60000),
            "-3m"
          ],
          [
            Ms(Ms("10 hours")),
            "10h"
          ]
        ];
        Jest2.testEach2("fromMS", fromMS_Tests, (function (result, expected) {
                expect(result).toEqual(expected);
              }));
        var fromMSLong_Tests = [
          [
            MS.fromMSLong(60000),
            "1 minute"
          ],
          [
            MS.fromMSLong(2 * 60000),
            "2 minutes"
          ],
          [
            MS.fromMSLong(-3 * 60000),
            "-3 minutes"
          ],
          [
            MS.fromMSLong(Ms("10 hours")),
            "10 hours"
          ]
        ];
        return Jest2.testEach2("fromMSLong", fromMSLong_Tests, (function (result, expected) {
                      expect(result).toEqual(expected);
                    }));
      }));

export {
  
}
/*  Not a pure module */
