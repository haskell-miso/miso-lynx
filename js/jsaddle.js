// ts/native-saddle.ts
globalThis["window"] = {};
globalThis["processData"] = function() {};
globalThis["renderPage"] = function() {
  console.log("connecting...");
  console.log("hi");
  connect();
};
var connect = function() {
  "background";
  console.log("in connect", lynx.getJSModule);
};
var runner = function(m) {
  console.log("inside of run");
  const { WebSocket } = m;
  console.log("inside connect");
  var wsaddress = "ws://192.168.1.239:8008/";
  console.log(lynx.getJSModule, lynx);
  var ws0 = new WebSocket(wsaddress);
  console.log("connected websocket...", ws0);
  var syncKey = "";
  ws0.onopen = function(e) {
    ws0.send("");
    var initialResults = [];
    var ws = { send: function(m2) {
      initialResults.push(m2);
    } };
    var jsaddle_values = new Map;
    var jsaddle_free = new Map;
    jsaddle_values.set(0, null);
    jsaddle_values.set(1, undefined);
    jsaddle_values.set(2, false);
    jsaddle_values.set(3, true);
    jsaddle_values.set(4, globalThis);
    var jsaddle_index = 100;
    var expectedBatch = 1;
    var lastResults = [0, { tag: "Success", contents: [[], []] }];
    var inCallback = 0;
    var asyncBatch = null;
    ws0.onmessage = function(e) {
      var batch = JSON.parse(e.data);
      if (inCallback > 0) {
        asyncBatch = batch;
        return;
      }
      if (typeof batch === "string") {
        syncKey = batch;
        var ws1 = new WebSocket(wsaddress);
        ws1.onopen = function(e2) {
          ws1.send(syncKey);
          initialResults.forEach(function(m2) {
            ws1.send(m2);
          });
          initialResults = null;
          ws = ws1;
        };
        var xhr = new XMLHttpRequest;
        xhr.open("POST", "/reload/" + syncKey, true);
        xhr.onreadystatechange = function() {
          if (xhr.readyState === XMLHttpRequest.DONE && xhr.status === 200)
            setTimeout(function() {
              window.location.reload();
            }, 100);
        };
        xhr.send();
        return;
      }
      var runBatch = function(firstBatch, initialSyncDepth) {
        var processBatch = function(timestamp) {
          var batch = firstBatch;
          var callbacksToFree = [];
          var results = [];
          inCallback++;
          try {
            syncDepth = initialSyncDepth || 0;
            for (;; ) {
              if (batch[2] === expectedBatch) {
                expectedBatch++;
                var nCommandsLength = batch[0].length;
                for (var nCommand = 0;nCommand != nCommandsLength; nCommand++) {
                  var cmd = batch[0][nCommand];
                  if (cmd.Left) {
                    var d = cmd.Left;
                    switch (d.tag) {
                      case "FreeRef":
                        var refsToFree = jsaddle_free.get(d.contents[0]) || [];
                        refsToFree.push(d.contents[1]);
                        jsaddle_free.set(d.contents[0], refsToFree);
                        break;
                      case "FreeRefs":
                        var refsToFree = jsaddle_free.get(d.contents) || [];
                        for (var nRef = 0;nRef != refsToFree.length; nRef++)
                          jsaddle_values.delete(refsToFree[nRef]);
                        jsaddle_free.delete(d.contents);
                        break;
                      case "SetPropertyByName":
                        jsaddle_values.get(d.contents[0])[d.contents[1]] = jsaddle_values.get(d.contents[2]);
                        break;
                      case "SetPropertyAtIndex":
                        jsaddle_values.get(d.contents[0])[d.contents[1]] = jsaddle_values.get(d.contents[2]);
                        break;
                      case "EvaluateScript":
                        var n = d.contents[1];
                        jsaddle_values.set(n, eval(d.contents[0]));
                        break;
                      case "StringToValue":
                        var n = d.contents[1];
                        jsaddle_values.set(n, d.contents[0]);
                        break;
                      case "JSONValueToValue":
                        var n = d.contents[1];
                        jsaddle_values.set(n, d.contents[0]);
                        break;
                      case "GetPropertyByName":
                        var n = d.contents[2];
                        jsaddle_values.set(n, jsaddle_values.get(d.contents[0])[d.contents[1]]);
                        break;
                      case "GetPropertyAtIndex":
                        var n = d.contents[2];
                        jsaddle_values.set(n, jsaddle_values.get(d.contents[0])[d.contents[1]]);
                        break;
                      case "NumberToValue":
                        var n = d.contents[1];
                        jsaddle_values.set(n, d.contents[0]);
                        break;
                      case "NewEmptyObject":
                        var n = d.contents;
                        jsaddle_values.set(n, {});
                        break;
                      case "NewAsyncCallback":
                        (function() {
                          var nFunction = d.contents;
                          var func = function() {
                            var nFunctionInFunc = ++jsaddle_index;
                            jsaddle_values.set(nFunctionInFunc, func);
                            var nThis = ++jsaddle_index;
                            jsaddle_values.set(nThis, this);
                            var args = [];
                            for (var i2 = 0;i2 != arguments.length; i2++) {
                              var nArg = ++jsaddle_index;
                              jsaddle_values.set(nArg, arguments[i2]);
                              args[i2] = nArg;
                            }
                            ws.send(JSON.stringify({ tag: "Callback", contents: [lastResults[0], lastResults[1], nFunction, nFunctionInFunc, nThis, args] }));
                          };
                          jsaddle_values.set(nFunction, func);
                        })();
                        break;
                      case "NewSyncCallback":
                        (function() {
                          var nFunction = d.contents;
                          var func = function() {
                            var nFunctionInFunc = ++jsaddle_index;
                            jsaddle_values.set(nFunctionInFunc, func);
                            var nThis = ++jsaddle_index;
                            jsaddle_values.set(nThis, this);
                            var args = [];
                            for (var i2 = 0;i2 != arguments.length; i2++) {
                              var nArg = ++jsaddle_index;
                              jsaddle_values.set(nArg, arguments[i2]);
                              args[i2] = nArg;
                            }
                            if (inCallback > 0) {
                              ws.send(JSON.stringify({ tag: "Callback", contents: [lastResults[0], lastResults[1], nFunction, nFunctionInFunc, nThis, args] }));
                            } else {
                              runBatch(function() {
                                var xhr2 = new XMLHttpRequest;
                                xhr2.open("POST", "/sync/" + syncKey, false);
                                xhr2.setRequestHeader("Content-type", "application/json");
                                xhr2.send(JSON.stringify({ tag: "Callback", contents: [lastResults[0], lastResults[1], nFunction, nFunctionInFunc, nThis, args] }));
                                return JSON.parse(xhr2.response);
                              }(), 1);
                            }
                          };
                          jsaddle_values.set(nFunction, func);
                        })();
                        break;
                      case "FreeCallback":
                        callbacksToFree.push(d.contents);
                        break;
                      case "CallAsFunction":
                        var n = d.contents[3];
                        jsaddle_values.set(n, jsaddle_values.get(d.contents[0]).apply(jsaddle_values.get(d.contents[1]), d.contents[2].map(function(arg) {
                          return jsaddle_values.get(arg);
                        })));
                        break;
                      case "CallAsConstructor":
                        var n = d.contents[2];
                        var r;
                        var f = jsaddle_values.get(d.contents[0]);
                        var a = d.contents[1].map(function(arg) {
                          return jsaddle_values.get(arg);
                        });
                        switch (a.length) {
                          case 0:
                            r = new f;
                            break;
                          case 1:
                            r = new f(a[0]);
                            break;
                          case 2:
                            r = new f(a[0], a[1]);
                            break;
                          case 3:
                            r = new f(a[0], a[1], a[2]);
                            break;
                          case 4:
                            r = new f(a[0], a[1], a[2], a[3]);
                            break;
                          case 5:
                            r = new f(a[0], a[1], a[2], a[3], a[4]);
                            break;
                          case 6:
                            r = new f(a[0], a[1], a[2], a[3], a[4], a[5]);
                            break;
                          case 7:
                            r = new f(a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
                            break;
                          default:
                            var ret;
                            var temp = function() {
                              ret = f.apply(this, a);
                            };
                            temp.prototype = f.prototype;
                            var i = new temp;
                            if (ret instanceof Object)
                              r = ret;
                            else {
                              i.constructor = f;
                              r = i;
                            }
                        }
                        jsaddle_values.set(n, r);
                        break;
                      case "NewArray":
                        var n = d.contents[1];
                        jsaddle_values.set(n, d.contents[0].map(function(v2) {
                          return jsaddle_values.get(v2);
                        }));
                        break;
                      case "SyncWithAnimationFrame":
                        var n = d.contents;
                        jsaddle_values.set(n, timestamp);
                        break;
                      case "StartSyncBlock":
                        syncDepth++;
                        break;
                      case "EndSyncBlock":
                        syncDepth--;
                        break;
                      default:
                        ws.send(JSON.stringify({ tag: "ProtocolError", contents: e.data }));
                        return;
                    }
                  } else {
                    var d = cmd.Right;
                    switch (d.tag) {
                      case "ValueToString":
                        var val = jsaddle_values.get(d.contents);
                        var s = val === null ? "null" : val === undefined ? "undefined" : val.toString();
                        results.push({ tag: "ValueToStringResult", contents: s });
                        break;
                      case "ValueToBool":
                        results.push({ tag: "ValueToBoolResult", contents: jsaddle_values.get(d.contents) ? true : false });
                        break;
                      case "ValueToNumber":
                        results.push({ tag: "ValueToNumberResult", contents: Number(jsaddle_values.get(d.contents)) });
                        break;
                      case "ValueToJSON":
                        var s = jsaddle_values.get(d.contents) === undefined ? "" : JSON.stringify(jsaddle_values.get(d.contents));
                        results.push({ tag: "ValueToJSONResult", contents: s });
                        break;
                      case "ValueToJSONValue":
                        results.push({ tag: "ValueToJSONValueResult", contents: jsaddle_values.get(d.contents) });
                        break;
                      case "DeRefVal":
                        var n = d.contents;
                        var v = jsaddle_values.get(n);
                        var c = v === null ? [0, ""] : v === undefined ? [1, ""] : v === false ? [2, ""] : v === true ? [3, ""] : typeof v === "number" ? [-1, v.toString()] : typeof v === "string" ? [-2, v] : [-3, ""];
                        results.push({ tag: "DeRefValResult", contents: c });
                        break;
                      case "IsNull":
                        results.push({ tag: "IsNullResult", contents: jsaddle_values.get(d.contents) === null });
                        break;
                      case "IsUndefined":
                        results.push({ tag: "IsUndefinedResult", contents: jsaddle_values.get(d.contents) === undefined });
                        break;
                      case "InstanceOf":
                        results.push({ tag: "InstanceOfResult", contents: jsaddle_values.get(d.contents[0]) instanceof jsaddle_values.get(d.contents[1]) });
                        break;
                      case "StrictEqual":
                        results.push({ tag: "StrictEqualResult", contents: jsaddle_values.get(d.contents[0]) === jsaddle_values.get(d.contents[1]) });
                        break;
                      case "PropertyNames":
                        var result = [];
                        for (name in jsaddle_values.get(d.contents)) {
                          result.push(name);
                        }
                        results.push({ tag: "PropertyNamesResult", contents: result });
                        break;
                      case "Sync":
                        results.push({ tag: "SyncResult", contents: [] });
                        break;
                      default:
                        results.push({ tag: "ProtocolError", contents: e.data });
                    }
                  }
                }
                if (syncDepth <= 0) {
                  lastResults = [batch[2], { tag: "Success", contents: [callbacksToFree, results] }];
                  ws.send(JSON.stringify({ tag: "BatchResults", contents: [lastResults[0], lastResults[1]] }));
                  break;
                } else {
                  lastResults = [batch[2], { tag: "Success", contents: [callbacksToFree, results] }];
                  batch = function() {
                    var xhr2 = new XMLHttpRequest;
                    xhr2.open("POST", "/sync/" + syncKey, false);
                    xhr2.setRequestHeader("Content-type", "application/json");
                    xhr2.send(JSON.stringify({ tag: "BatchResults", contents: [lastResults[0], lastResults[1]] }));
                    return JSON.parse(xhr2.response);
                  }();
                  results = [];
                  callbacksToFree = [];
                }
              } else {
                if (syncDepth <= 0) {
                  break;
                } else {
                  if (batch[2] === expectedBatch - 1) {
                    batch = function() {
                      var xhr2 = new XMLHttpRequest;
                      xhr2.open("POST", "/sync/" + syncKey, false);
                      xhr2.setRequestHeader("Content-type", "application/json");
                      xhr2.send(JSON.stringify({ tag: "BatchResults", contents: [lastResults[0], lastResults[1]] }));
                      return JSON.parse(xhr2.response);
                    }();
                  } else {
                    batch = function() {
                      var xhr2 = new XMLHttpRequest;
                      xhr2.open("POST", "/sync/" + syncKey, false);
                      xhr2.setRequestHeader("Content-type", "application/json");
                      xhr2.send(JSON.stringify({ tag: "Duplicate", contents: [batch[2], expectedBatch] }));
                      return JSON.parse(xhr2.response);
                    }();
                  }
                  results = [];
                  callbacksToFree = [];
                }
              }
            }
          } catch (err) {
            var n = ++jsaddle_index;
            jsaddle_values.set(n, err);
            console.log(err);
            ws.send(JSON.stringify({ tag: "BatchResults", contents: [batch[2], { tag: "Failure", contents: [callbacksToFree, results, n, String(err)] }] }));
          }
          if (inCallback == 1) {
            while (asyncBatch !== null) {
              var b = asyncBatch;
              asyncBatch = null;
              if (b[2] == expectedBatch)
                runBatch(b);
            }
          }
          inCallback--;
        };
        if (batch[1] && (initialSyncDepth || 0) === 0) {
          window.requestAnimationFrame(processBatch);
        } else {
          processBatch(window.performance ? window.performance.now() : null);
        }
      };
      runBatch(batch);
    };
  };
  ws0.onerror = function() {
    setTimeout(connect, 1000);
  };
};
function h$isNumber(o) {
  return typeof o === "number";
}
function h$isObject(o) {
  return typeof o === "object";
}
function h$isString(o) {
  return typeof o === "string";
}
function h$isSymbol(o) {
  return typeof o === "symbol";
}
function h$isBoolean(o) {
  return typeof o === "boolean";
}
function h$isFunction(o) {
  return typeof o === "function";
}
function h$jsTypeOf(o) {
  var t = typeof o;
  if (t === "undefined")
    return 0;
  if (t === "object")
    return 1;
  if (t === "boolean")
    return 2;
  if (t === "number")
    return 3;
  if (t === "string")
    return 4;
  if (t === "symbol")
    return 5;
  if (t === "function")
    return 6;
  return 7;
}
function h$jsonTypeOf(o) {
  if (!(o instanceof Object)) {
    if (o == null) {
      return 0;
    } else if (typeof o == "number") {
      if (h$isInteger(o)) {
        return 1;
      } else {
        return 2;
      }
    } else if (typeof o == "boolean") {
      return 3;
    } else {
      return 4;
    }
  } else {
    if (Object.prototype.toString.call(o) == "[object Array]") {
      return 5;
    } else if (!o) {
      return 0;
    } else {
      return 6;
    }
  }
}
function h$roundUpToMultipleOf(n2, m2) {
  var rem = n2 % m2;
  return rem === 0 ? n2 : n2 - rem + m2;
}
function h$newByteArray(len) {
  var len0 = Math.max(h$roundUpToMultipleOf(len, 8), 8);
  var buf = new ArrayBuffer(len0);
  return {
    buf,
    len,
    i3: new Int32Array(buf),
    u8: new Uint8Array(buf),
    u1: new Uint16Array(buf),
    f3: new Float32Array(buf),
    f6: new Float64Array(buf),
    dv: new DataView(buf)
  };
}
function h$wrapBuffer(buf, unalignedOk, offset, length) {
  if (!unalignedOk && offset && offset % 8 !== 0) {
    throw "h$wrapBuffer: offset not aligned:" + offset;
  }
  if (!buf || !(buf instanceof ArrayBuffer))
    throw "h$wrapBuffer: not an ArrayBuffer";
  if (!offset) {
    offset = 0;
  }
  if (!length || length < 0) {
    length = buf.byteLength - offset;
  }
  return {
    buf,
    len: length,
    i3: offset % 4 ? null : new Int32Array(buf, offset, length >> 2),
    u8: new Uint8Array(buf, offset, length),
    u1: offset % 2 ? null : new Uint16Array(buf, offset, length >> 1),
    f3: offset % 4 ? null : new Float32Array(buf, offset, length >> 2),
    f6: offset % 8 ? null : new Float64Array(buf, offset, length >> 3),
    dv: new DataView(buf, offset, length)
  };
}
function h$newByteArrayFromBase64String(base64) {
  var bin = window.atob(base64);
  var ba = h$newByteArray(bin.length);
  var u8 = ba.u8;
  for (var i2 = 0;i2 < bin.length; i2++) {
    u8[i2] = bin.charCodeAt(i2);
  }
  return ba;
}
function h$byteArrayToBase64String(off, len, ba) {
  var bin = "";
  var u8 = ba.u8;
  var end = off + len;
  for (var i2 = off;i2 < end; i2++) {
    bin += String.fromCharCode(u8[i2]);
  }
  return window.btoa(bin);
}
