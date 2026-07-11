(() => {
  var __create = Object.create;
  var __defProp = Object.defineProperty;
  var __getOwnPropDesc = Object.getOwnPropertyDescriptor;
  var __getOwnPropNames = Object.getOwnPropertyNames;
  var __getProtoOf = Object.getPrototypeOf;
  var __hasOwnProp = Object.prototype.hasOwnProperty;
  var __commonJS = (cb, mod2) => function __require() {
    return mod2 || (0, cb[__getOwnPropNames(cb)[0]])((mod2 = { exports: {} }).exports, mod2), mod2.exports;
  };
  var __copyProps = (to, from, except, desc) => {
    if (from && typeof from === "object" || typeof from === "function") {
      for (let key of __getOwnPropNames(from))
        if (!__hasOwnProp.call(to, key) && key !== except)
          __defProp(to, key, { get: () => from[key], enumerable: !(desc = __getOwnPropDesc(from, key)) || desc.enumerable });
    }
    return to;
  };
  var __toESM = (mod2, isNodeMode, target) => (target = mod2 != null ? __create(__getProtoOf(mod2)) : {}, __copyProps(
    // If the importer is in node compatibility mode or this is not an ESM
    // file that has been converted to a CommonJS file using a Babel-
    // compatible transform (i.e. "__esModule" has not been set), then set
    // "default" to the CommonJS "module.exports" for node compatibility.
    isNodeMode || !mod2 || !mod2.__esModule ? __defProp(target, "default", { value: mod2, enumerable: true }) : target,
    mod2
  ));

  // ../../../../../../../../Dropbox/projects/hazel/node_modules/big-integer/BigInteger.js
  var require_BigInteger = __commonJS({
    "../../../../../../../../Dropbox/projects/hazel/node_modules/big-integer/BigInteger.js"(exports2, module2) {
      var bigInt2 = function(undefined2) {
        "use strict";
        var BASE = 1e7, LOG_BASE = 7, MAX_INT = 9007199254740992, MAX_INT_ARR = smallToArray(MAX_INT), DEFAULT_ALPHABET = "0123456789abcdefghijklmnopqrstuvwxyz";
        var supportsNativeBigInt = typeof BigInt === "function";
        function Integer(v2, radix, alphabet, caseSensitive) {
          if (typeof v2 === "undefined") return Integer[0];
          if (typeof radix !== "undefined") return +radix === 10 && !alphabet ? parseValue(v2) : parseBase(v2, radix, alphabet, caseSensitive);
          return parseValue(v2);
        }
        function BigInteger(value, sign2) {
          this.value = value;
          this.sign = sign2;
          this.isSmall = false;
        }
        BigInteger.prototype = Object.create(Integer.prototype);
        function SmallInteger(value) {
          this.value = value;
          this.sign = value < 0;
          this.isSmall = true;
        }
        SmallInteger.prototype = Object.create(Integer.prototype);
        function NativeBigInt(value) {
          this.value = value;
        }
        NativeBigInt.prototype = Object.create(Integer.prototype);
        function isPrecise(n9) {
          return -MAX_INT < n9 && n9 < MAX_INT;
        }
        function smallToArray(n9) {
          if (n9 < 1e7)
            return [n9];
          if (n9 < 1e14)
            return [n9 % 1e7, Math.floor(n9 / 1e7)];
          return [n9 % 1e7, Math.floor(n9 / 1e7) % 1e7, Math.floor(n9 / 1e14)];
        }
        function arrayToSmall(arr) {
          trim(arr);
          var length2 = arr.length;
          if (length2 < 4 && compareAbs(arr, MAX_INT_ARR) < 0) {
            switch (length2) {
              case 0:
                return 0;
              case 1:
                return arr[0];
              case 2:
                return arr[0] + arr[1] * BASE;
              default:
                return arr[0] + (arr[1] + arr[2] * BASE) * BASE;
            }
          }
          return arr;
        }
        function trim(v2) {
          var i6 = v2.length;
          while (v2[--i6] === 0) ;
          v2.length = i6 + 1;
        }
        function createArray(length2) {
          var x2 = new Array(length2);
          var i6 = -1;
          while (++i6 < length2) {
            x2[i6] = 0;
          }
          return x2;
        }
        function truncate(n9) {
          if (n9 > 0) return Math.floor(n9);
          return Math.ceil(n9);
        }
        function add2(a4, b2) {
          var l_a = a4.length, l_b = b2.length, r6 = new Array(l_a), carry = 0, base = BASE, sum, i6;
          for (i6 = 0; i6 < l_b; i6++) {
            sum = a4[i6] + b2[i6] + carry;
            carry = sum >= base ? 1 : 0;
            r6[i6] = sum - carry * base;
          }
          while (i6 < l_a) {
            sum = a4[i6] + carry;
            carry = sum === base ? 1 : 0;
            r6[i6++] = sum - carry * base;
          }
          if (carry > 0) r6.push(carry);
          return r6;
        }
        function addAny(a4, b2) {
          if (a4.length >= b2.length) return add2(a4, b2);
          return add2(b2, a4);
        }
        function addSmall(a4, carry) {
          var l8 = a4.length, r6 = new Array(l8), base = BASE, sum, i6;
          for (i6 = 0; i6 < l8; i6++) {
            sum = a4[i6] - base + carry;
            carry = Math.floor(sum / base);
            r6[i6] = sum - carry * base;
            carry += 1;
          }
          while (carry > 0) {
            r6[i6++] = carry % base;
            carry = Math.floor(carry / base);
          }
          return r6;
        }
        BigInteger.prototype.add = function(v2) {
          var n9 = parseValue(v2);
          if (this.sign !== n9.sign) {
            return this.subtract(n9.negate());
          }
          var a4 = this.value, b2 = n9.value;
          if (n9.isSmall) {
            return new BigInteger(addSmall(a4, Math.abs(b2)), this.sign);
          }
          return new BigInteger(addAny(a4, b2), this.sign);
        };
        BigInteger.prototype.plus = BigInteger.prototype.add;
        SmallInteger.prototype.add = function(v2) {
          var n9 = parseValue(v2);
          var a4 = this.value;
          if (a4 < 0 !== n9.sign) {
            return this.subtract(n9.negate());
          }
          var b2 = n9.value;
          if (n9.isSmall) {
            if (isPrecise(a4 + b2)) return new SmallInteger(a4 + b2);
            b2 = smallToArray(Math.abs(b2));
          }
          return new BigInteger(addSmall(b2, Math.abs(a4)), a4 < 0);
        };
        SmallInteger.prototype.plus = SmallInteger.prototype.add;
        NativeBigInt.prototype.add = function(v2) {
          return new NativeBigInt(this.value + parseValue(v2).value);
        };
        NativeBigInt.prototype.plus = NativeBigInt.prototype.add;
        function subtract2(a4, b2) {
          var a_l = a4.length, b_l = b2.length, r6 = new Array(a_l), borrow = 0, base = BASE, i6, difference;
          for (i6 = 0; i6 < b_l; i6++) {
            difference = a4[i6] - borrow - b2[i6];
            if (difference < 0) {
              difference += base;
              borrow = 1;
            } else borrow = 0;
            r6[i6] = difference;
          }
          for (i6 = b_l; i6 < a_l; i6++) {
            difference = a4[i6] - borrow;
            if (difference < 0) difference += base;
            else {
              r6[i6++] = difference;
              break;
            }
            r6[i6] = difference;
          }
          for (; i6 < a_l; i6++) {
            r6[i6] = a4[i6];
          }
          trim(r6);
          return r6;
        }
        function subtractAny(a4, b2, sign2) {
          var value;
          if (compareAbs(a4, b2) >= 0) {
            value = subtract2(a4, b2);
          } else {
            value = subtract2(b2, a4);
            sign2 = !sign2;
          }
          value = arrayToSmall(value);
          if (typeof value === "number") {
            if (sign2) value = -value;
            return new SmallInteger(value);
          }
          return new BigInteger(value, sign2);
        }
        function subtractSmall(a4, b2, sign2) {
          var l8 = a4.length, r6 = new Array(l8), carry = -b2, base = BASE, i6, difference;
          for (i6 = 0; i6 < l8; i6++) {
            difference = a4[i6] + carry;
            carry = Math.floor(difference / base);
            difference %= base;
            r6[i6] = difference < 0 ? difference + base : difference;
          }
          r6 = arrayToSmall(r6);
          if (typeof r6 === "number") {
            if (sign2) r6 = -r6;
            return new SmallInteger(r6);
          }
          return new BigInteger(r6, sign2);
        }
        BigInteger.prototype.subtract = function(v2) {
          var n9 = parseValue(v2);
          if (this.sign !== n9.sign) {
            return this.add(n9.negate());
          }
          var a4 = this.value, b2 = n9.value;
          if (n9.isSmall)
            return subtractSmall(a4, Math.abs(b2), this.sign);
          return subtractAny(a4, b2, this.sign);
        };
        BigInteger.prototype.minus = BigInteger.prototype.subtract;
        SmallInteger.prototype.subtract = function(v2) {
          var n9 = parseValue(v2);
          var a4 = this.value;
          if (a4 < 0 !== n9.sign) {
            return this.add(n9.negate());
          }
          var b2 = n9.value;
          if (n9.isSmall) {
            return new SmallInteger(a4 - b2);
          }
          return subtractSmall(b2, Math.abs(a4), a4 >= 0);
        };
        SmallInteger.prototype.minus = SmallInteger.prototype.subtract;
        NativeBigInt.prototype.subtract = function(v2) {
          return new NativeBigInt(this.value - parseValue(v2).value);
        };
        NativeBigInt.prototype.minus = NativeBigInt.prototype.subtract;
        BigInteger.prototype.negate = function() {
          return new BigInteger(this.value, !this.sign);
        };
        SmallInteger.prototype.negate = function() {
          var sign2 = this.sign;
          var small = new SmallInteger(-this.value);
          small.sign = !sign2;
          return small;
        };
        NativeBigInt.prototype.negate = function() {
          return new NativeBigInt(-this.value);
        };
        BigInteger.prototype.abs = function() {
          return new BigInteger(this.value, false);
        };
        SmallInteger.prototype.abs = function() {
          return new SmallInteger(Math.abs(this.value));
        };
        NativeBigInt.prototype.abs = function() {
          return new NativeBigInt(this.value >= 0 ? this.value : -this.value);
        };
        function multiplyLong(a4, b2) {
          var a_l = a4.length, b_l = b2.length, l8 = a_l + b_l, r6 = createArray(l8), base = BASE, product, carry, i6, a_i, b_j;
          for (i6 = 0; i6 < a_l; ++i6) {
            a_i = a4[i6];
            for (var j2 = 0; j2 < b_l; ++j2) {
              b_j = b2[j2];
              product = a_i * b_j + r6[i6 + j2];
              carry = Math.floor(product / base);
              r6[i6 + j2] = product - carry * base;
              r6[i6 + j2 + 1] += carry;
            }
          }
          trim(r6);
          return r6;
        }
        function multiplySmall(a4, b2) {
          var l8 = a4.length, r6 = new Array(l8), base = BASE, carry = 0, product, i6;
          for (i6 = 0; i6 < l8; i6++) {
            product = a4[i6] * b2 + carry;
            carry = Math.floor(product / base);
            r6[i6] = product - carry * base;
          }
          while (carry > 0) {
            r6[i6++] = carry % base;
            carry = Math.floor(carry / base);
          }
          return r6;
        }
        function shiftLeft(x2, n9) {
          var r6 = [];
          while (n9-- > 0) r6.push(0);
          return r6.concat(x2);
        }
        function multiplyKaratsuba(x2, y2) {
          var n9 = Math.max(x2.length, y2.length);
          if (n9 <= 30) return multiplyLong(x2, y2);
          n9 = Math.ceil(n9 / 2);
          var b2 = x2.slice(n9), a4 = x2.slice(0, n9), d3 = y2.slice(n9), c6 = y2.slice(0, n9);
          var ac = multiplyKaratsuba(a4, c6), bd = multiplyKaratsuba(b2, d3), abcd = multiplyKaratsuba(addAny(a4, b2), addAny(c6, d3));
          var product = addAny(addAny(ac, shiftLeft(subtract2(subtract2(abcd, ac), bd), n9)), shiftLeft(bd, 2 * n9));
          trim(product);
          return product;
        }
        function useKaratsuba(l1, l22) {
          return -0.012 * l1 - 0.012 * l22 + 15e-6 * l1 * l22 > 0;
        }
        BigInteger.prototype.multiply = function(v2) {
          var n9 = parseValue(v2), a4 = this.value, b2 = n9.value, sign2 = this.sign !== n9.sign, abs2;
          if (n9.isSmall) {
            if (b2 === 0) return Integer[0];
            if (b2 === 1) return this;
            if (b2 === -1) return this.negate();
            abs2 = Math.abs(b2);
            if (abs2 < BASE) {
              return new BigInteger(multiplySmall(a4, abs2), sign2);
            }
            b2 = smallToArray(abs2);
          }
          if (useKaratsuba(a4.length, b2.length))
            return new BigInteger(multiplyKaratsuba(a4, b2), sign2);
          return new BigInteger(multiplyLong(a4, b2), sign2);
        };
        BigInteger.prototype.times = BigInteger.prototype.multiply;
        function multiplySmallAndArray(a4, b2, sign2) {
          if (a4 < BASE) {
            return new BigInteger(multiplySmall(b2, a4), sign2);
          }
          return new BigInteger(multiplyLong(b2, smallToArray(a4)), sign2);
        }
        SmallInteger.prototype._multiplyBySmall = function(a4) {
          if (isPrecise(a4.value * this.value)) {
            return new SmallInteger(a4.value * this.value);
          }
          return multiplySmallAndArray(Math.abs(a4.value), smallToArray(Math.abs(this.value)), this.sign !== a4.sign);
        };
        BigInteger.prototype._multiplyBySmall = function(a4) {
          if (a4.value === 0) return Integer[0];
          if (a4.value === 1) return this;
          if (a4.value === -1) return this.negate();
          return multiplySmallAndArray(Math.abs(a4.value), this.value, this.sign !== a4.sign);
        };
        SmallInteger.prototype.multiply = function(v2) {
          return parseValue(v2)._multiplyBySmall(this);
        };
        SmallInteger.prototype.times = SmallInteger.prototype.multiply;
        NativeBigInt.prototype.multiply = function(v2) {
          return new NativeBigInt(this.value * parseValue(v2).value);
        };
        NativeBigInt.prototype.times = NativeBigInt.prototype.multiply;
        function square2(a4) {
          var l8 = a4.length, r6 = createArray(l8 + l8), base = BASE, product, carry, i6, a_i, a_j;
          for (i6 = 0; i6 < l8; i6++) {
            a_i = a4[i6];
            carry = 0 - a_i * a_i;
            for (var j2 = i6; j2 < l8; j2++) {
              a_j = a4[j2];
              product = 2 * (a_i * a_j) + r6[i6 + j2] + carry;
              carry = Math.floor(product / base);
              r6[i6 + j2] = product - carry * base;
            }
            r6[i6 + l8] = carry;
          }
          trim(r6);
          return r6;
        }
        BigInteger.prototype.square = function() {
          return new BigInteger(square2(this.value), false);
        };
        SmallInteger.prototype.square = function() {
          var value = this.value * this.value;
          if (isPrecise(value)) return new SmallInteger(value);
          return new BigInteger(square2(smallToArray(Math.abs(this.value))), false);
        };
        NativeBigInt.prototype.square = function(v2) {
          return new NativeBigInt(this.value * this.value);
        };
        function divMod1(a4, b2) {
          var a_l = a4.length, b_l = b2.length, base = BASE, result = createArray(b2.length), divisorMostSignificantDigit = b2[b_l - 1], lambda = Math.ceil(base / (2 * divisorMostSignificantDigit)), remainder = multiplySmall(a4, lambda), divisor = multiplySmall(b2, lambda), quotientDigit, shift, carry, borrow, i6, l8, q;
          if (remainder.length <= a_l) remainder.push(0);
          divisor.push(0);
          divisorMostSignificantDigit = divisor[b_l - 1];
          for (shift = a_l - b_l; shift >= 0; shift--) {
            quotientDigit = base - 1;
            if (remainder[shift + b_l] !== divisorMostSignificantDigit) {
              quotientDigit = Math.floor((remainder[shift + b_l] * base + remainder[shift + b_l - 1]) / divisorMostSignificantDigit);
            }
            carry = 0;
            borrow = 0;
            l8 = divisor.length;
            for (i6 = 0; i6 < l8; i6++) {
              carry += quotientDigit * divisor[i6];
              q = Math.floor(carry / base);
              borrow += remainder[shift + i6] - (carry - q * base);
              carry = q;
              if (borrow < 0) {
                remainder[shift + i6] = borrow + base;
                borrow = -1;
              } else {
                remainder[shift + i6] = borrow;
                borrow = 0;
              }
            }
            while (borrow !== 0) {
              quotientDigit -= 1;
              carry = 0;
              for (i6 = 0; i6 < l8; i6++) {
                carry += remainder[shift + i6] - base + divisor[i6];
                if (carry < 0) {
                  remainder[shift + i6] = carry + base;
                  carry = 0;
                } else {
                  remainder[shift + i6] = carry;
                  carry = 1;
                }
              }
              borrow += carry;
            }
            result[shift] = quotientDigit;
          }
          remainder = divModSmall(remainder, lambda)[0];
          return [arrayToSmall(result), arrayToSmall(remainder)];
        }
        function divMod2(a4, b2) {
          var a_l = a4.length, b_l = b2.length, result = [], part = [], base = BASE, guess2, xlen, highx, highy, check;
          while (a_l) {
            part.unshift(a4[--a_l]);
            trim(part);
            if (compareAbs(part, b2) < 0) {
              result.push(0);
              continue;
            }
            xlen = part.length;
            highx = part[xlen - 1] * base + part[xlen - 2];
            highy = b2[b_l - 1] * base + b2[b_l - 2];
            if (xlen > b_l) {
              highx = (highx + 1) * base;
            }
            guess2 = Math.ceil(highx / highy);
            do {
              check = multiplySmall(b2, guess2);
              if (compareAbs(check, part) <= 0) break;
              guess2--;
            } while (guess2);
            result.push(guess2);
            part = subtract2(part, check);
          }
          result.reverse();
          return [arrayToSmall(result), arrayToSmall(part)];
        }
        function divModSmall(value, lambda) {
          var length2 = value.length, quotient = createArray(length2), base = BASE, i6, q, remainder, divisor;
          remainder = 0;
          for (i6 = length2 - 1; i6 >= 0; --i6) {
            divisor = remainder * base + value[i6];
            q = truncate(divisor / lambda);
            remainder = divisor - q * lambda;
            quotient[i6] = q | 0;
          }
          return [quotient, remainder | 0];
        }
        function divModAny(self, v2) {
          var value, n9 = parseValue(v2);
          if (supportsNativeBigInt) {
            return [new NativeBigInt(self.value / n9.value), new NativeBigInt(self.value % n9.value)];
          }
          var a4 = self.value, b2 = n9.value;
          var quotient;
          if (b2 === 0) throw new Error("Cannot divide by zero");
          if (self.isSmall) {
            if (n9.isSmall) {
              return [new SmallInteger(truncate(a4 / b2)), new SmallInteger(a4 % b2)];
            }
            return [Integer[0], self];
          }
          if (n9.isSmall) {
            if (b2 === 1) return [self, Integer[0]];
            if (b2 == -1) return [self.negate(), Integer[0]];
            var abs2 = Math.abs(b2);
            if (abs2 < BASE) {
              value = divModSmall(a4, abs2);
              quotient = arrayToSmall(value[0]);
              var remainder = value[1];
              if (self.sign) remainder = -remainder;
              if (typeof quotient === "number") {
                if (self.sign !== n9.sign) quotient = -quotient;
                return [new SmallInteger(quotient), new SmallInteger(remainder)];
              }
              return [new BigInteger(quotient, self.sign !== n9.sign), new SmallInteger(remainder)];
            }
            b2 = smallToArray(abs2);
          }
          var comparison = compareAbs(a4, b2);
          if (comparison === -1) return [Integer[0], self];
          if (comparison === 0) return [Integer[self.sign === n9.sign ? 1 : -1], Integer[0]];
          if (a4.length + b2.length <= 200)
            value = divMod1(a4, b2);
          else value = divMod2(a4, b2);
          quotient = value[0];
          var qSign = self.sign !== n9.sign, mod2 = value[1], mSign = self.sign;
          if (typeof quotient === "number") {
            if (qSign) quotient = -quotient;
            quotient = new SmallInteger(quotient);
          } else quotient = new BigInteger(quotient, qSign);
          if (typeof mod2 === "number") {
            if (mSign) mod2 = -mod2;
            mod2 = new SmallInteger(mod2);
          } else mod2 = new BigInteger(mod2, mSign);
          return [quotient, mod2];
        }
        BigInteger.prototype.divmod = function(v2) {
          var result = divModAny(this, v2);
          return {
            quotient: result[0],
            remainder: result[1]
          };
        };
        NativeBigInt.prototype.divmod = SmallInteger.prototype.divmod = BigInteger.prototype.divmod;
        BigInteger.prototype.divide = function(v2) {
          return divModAny(this, v2)[0];
        };
        NativeBigInt.prototype.over = NativeBigInt.prototype.divide = function(v2) {
          return new NativeBigInt(this.value / parseValue(v2).value);
        };
        SmallInteger.prototype.over = SmallInteger.prototype.divide = BigInteger.prototype.over = BigInteger.prototype.divide;
        BigInteger.prototype.mod = function(v2) {
          return divModAny(this, v2)[1];
        };
        NativeBigInt.prototype.mod = NativeBigInt.prototype.remainder = function(v2) {
          return new NativeBigInt(this.value % parseValue(v2).value);
        };
        SmallInteger.prototype.remainder = SmallInteger.prototype.mod = BigInteger.prototype.remainder = BigInteger.prototype.mod;
        BigInteger.prototype.pow = function(v2) {
          var n9 = parseValue(v2), a4 = this.value, b2 = n9.value, value, x2, y2;
          if (b2 === 0) return Integer[1];
          if (a4 === 0) return Integer[0];
          if (a4 === 1) return Integer[1];
          if (a4 === -1) return n9.isEven() ? Integer[1] : Integer[-1];
          if (n9.sign) {
            return Integer[0];
          }
          if (!n9.isSmall) throw new Error("The exponent " + n9.toString() + " is too large.");
          if (this.isSmall) {
            if (isPrecise(value = Math.pow(a4, b2)))
              return new SmallInteger(truncate(value));
          }
          x2 = this;
          y2 = Integer[1];
          while (true) {
            if (b2 & true) {
              y2 = y2.times(x2);
              --b2;
            }
            if (b2 === 0) break;
            b2 /= 2;
            x2 = x2.square();
          }
          return y2;
        };
        SmallInteger.prototype.pow = BigInteger.prototype.pow;
        NativeBigInt.prototype.pow = function(v2) {
          var n9 = parseValue(v2);
          var a4 = this.value, b2 = n9.value;
          var _0 = BigInt(0), _1 = BigInt(1), _2 = BigInt(2);
          if (b2 === _0) return Integer[1];
          if (a4 === _0) return Integer[0];
          if (a4 === _1) return Integer[1];
          if (a4 === BigInt(-1)) return n9.isEven() ? Integer[1] : Integer[-1];
          if (n9.isNegative()) return new NativeBigInt(_0);
          var x2 = this;
          var y2 = Integer[1];
          while (true) {
            if ((b2 & _1) === _1) {
              y2 = y2.times(x2);
              --b2;
            }
            if (b2 === _0) break;
            b2 /= _2;
            x2 = x2.square();
          }
          return y2;
        };
        BigInteger.prototype.modPow = function(exp, mod2) {
          exp = parseValue(exp);
          mod2 = parseValue(mod2);
          if (mod2.isZero()) throw new Error("Cannot take modPow with modulus 0");
          var r6 = Integer[1], base = this.mod(mod2);
          if (exp.isNegative()) {
            exp = exp.multiply(Integer[-1]);
            base = base.modInv(mod2);
          }
          while (exp.isPositive()) {
            if (base.isZero()) return Integer[0];
            if (exp.isOdd()) r6 = r6.multiply(base).mod(mod2);
            exp = exp.divide(2);
            base = base.square().mod(mod2);
          }
          return r6;
        };
        NativeBigInt.prototype.modPow = SmallInteger.prototype.modPow = BigInteger.prototype.modPow;
        function compareAbs(a4, b2) {
          if (a4.length !== b2.length) {
            return a4.length > b2.length ? 1 : -1;
          }
          for (var i6 = a4.length - 1; i6 >= 0; i6--) {
            if (a4[i6] !== b2[i6]) return a4[i6] > b2[i6] ? 1 : -1;
          }
          return 0;
        }
        BigInteger.prototype.compareAbs = function(v2) {
          var n9 = parseValue(v2), a4 = this.value, b2 = n9.value;
          if (n9.isSmall) return 1;
          return compareAbs(a4, b2);
        };
        SmallInteger.prototype.compareAbs = function(v2) {
          var n9 = parseValue(v2), a4 = Math.abs(this.value), b2 = n9.value;
          if (n9.isSmall) {
            b2 = Math.abs(b2);
            return a4 === b2 ? 0 : a4 > b2 ? 1 : -1;
          }
          return -1;
        };
        NativeBigInt.prototype.compareAbs = function(v2) {
          var a4 = this.value;
          var b2 = parseValue(v2).value;
          a4 = a4 >= 0 ? a4 : -a4;
          b2 = b2 >= 0 ? b2 : -b2;
          return a4 === b2 ? 0 : a4 > b2 ? 1 : -1;
        };
        BigInteger.prototype.compare = function(v2) {
          if (v2 === Infinity) {
            return -1;
          }
          if (v2 === -Infinity) {
            return 1;
          }
          var n9 = parseValue(v2), a4 = this.value, b2 = n9.value;
          if (this.sign !== n9.sign) {
            return n9.sign ? 1 : -1;
          }
          if (n9.isSmall) {
            return this.sign ? -1 : 1;
          }
          return compareAbs(a4, b2) * (this.sign ? -1 : 1);
        };
        BigInteger.prototype.compareTo = BigInteger.prototype.compare;
        SmallInteger.prototype.compare = function(v2) {
          if (v2 === Infinity) {
            return -1;
          }
          if (v2 === -Infinity) {
            return 1;
          }
          var n9 = parseValue(v2), a4 = this.value, b2 = n9.value;
          if (n9.isSmall) {
            return a4 == b2 ? 0 : a4 > b2 ? 1 : -1;
          }
          if (a4 < 0 !== n9.sign) {
            return a4 < 0 ? -1 : 1;
          }
          return a4 < 0 ? 1 : -1;
        };
        SmallInteger.prototype.compareTo = SmallInteger.prototype.compare;
        NativeBigInt.prototype.compare = function(v2) {
          if (v2 === Infinity) {
            return -1;
          }
          if (v2 === -Infinity) {
            return 1;
          }
          var a4 = this.value;
          var b2 = parseValue(v2).value;
          return a4 === b2 ? 0 : a4 > b2 ? 1 : -1;
        };
        NativeBigInt.prototype.compareTo = NativeBigInt.prototype.compare;
        BigInteger.prototype.equals = function(v2) {
          return this.compare(v2) === 0;
        };
        NativeBigInt.prototype.eq = NativeBigInt.prototype.equals = SmallInteger.prototype.eq = SmallInteger.prototype.equals = BigInteger.prototype.eq = BigInteger.prototype.equals;
        BigInteger.prototype.notEquals = function(v2) {
          return this.compare(v2) !== 0;
        };
        NativeBigInt.prototype.neq = NativeBigInt.prototype.notEquals = SmallInteger.prototype.neq = SmallInteger.prototype.notEquals = BigInteger.prototype.neq = BigInteger.prototype.notEquals;
        BigInteger.prototype.greater = function(v2) {
          return this.compare(v2) > 0;
        };
        NativeBigInt.prototype.gt = NativeBigInt.prototype.greater = SmallInteger.prototype.gt = SmallInteger.prototype.greater = BigInteger.prototype.gt = BigInteger.prototype.greater;
        BigInteger.prototype.lesser = function(v2) {
          return this.compare(v2) < 0;
        };
        NativeBigInt.prototype.lt = NativeBigInt.prototype.lesser = SmallInteger.prototype.lt = SmallInteger.prototype.lesser = BigInteger.prototype.lt = BigInteger.prototype.lesser;
        BigInteger.prototype.greaterOrEquals = function(v2) {
          return this.compare(v2) >= 0;
        };
        NativeBigInt.prototype.geq = NativeBigInt.prototype.greaterOrEquals = SmallInteger.prototype.geq = SmallInteger.prototype.greaterOrEquals = BigInteger.prototype.geq = BigInteger.prototype.greaterOrEquals;
        BigInteger.prototype.lesserOrEquals = function(v2) {
          return this.compare(v2) <= 0;
        };
        NativeBigInt.prototype.leq = NativeBigInt.prototype.lesserOrEquals = SmallInteger.prototype.leq = SmallInteger.prototype.lesserOrEquals = BigInteger.prototype.leq = BigInteger.prototype.lesserOrEquals;
        BigInteger.prototype.isEven = function() {
          return (this.value[0] & 1) === 0;
        };
        SmallInteger.prototype.isEven = function() {
          return (this.value & 1) === 0;
        };
        NativeBigInt.prototype.isEven = function() {
          return (this.value & BigInt(1)) === BigInt(0);
        };
        BigInteger.prototype.isOdd = function() {
          return (this.value[0] & 1) === 1;
        };
        SmallInteger.prototype.isOdd = function() {
          return (this.value & 1) === 1;
        };
        NativeBigInt.prototype.isOdd = function() {
          return (this.value & BigInt(1)) === BigInt(1);
        };
        BigInteger.prototype.isPositive = function() {
          return !this.sign;
        };
        SmallInteger.prototype.isPositive = function() {
          return this.value > 0;
        };
        NativeBigInt.prototype.isPositive = SmallInteger.prototype.isPositive;
        BigInteger.prototype.isNegative = function() {
          return this.sign;
        };
        SmallInteger.prototype.isNegative = function() {
          return this.value < 0;
        };
        NativeBigInt.prototype.isNegative = SmallInteger.prototype.isNegative;
        BigInteger.prototype.isUnit = function() {
          return false;
        };
        SmallInteger.prototype.isUnit = function() {
          return Math.abs(this.value) === 1;
        };
        NativeBigInt.prototype.isUnit = function() {
          return this.abs().value === BigInt(1);
        };
        BigInteger.prototype.isZero = function() {
          return false;
        };
        SmallInteger.prototype.isZero = function() {
          return this.value === 0;
        };
        NativeBigInt.prototype.isZero = function() {
          return this.value === BigInt(0);
        };
        BigInteger.prototype.isDivisibleBy = function(v2) {
          var n9 = parseValue(v2);
          if (n9.isZero()) return false;
          if (n9.isUnit()) return true;
          if (n9.compareAbs(2) === 0) return this.isEven();
          return this.mod(n9).isZero();
        };
        NativeBigInt.prototype.isDivisibleBy = SmallInteger.prototype.isDivisibleBy = BigInteger.prototype.isDivisibleBy;
        function isBasicPrime(v2) {
          var n9 = v2.abs();
          if (n9.isUnit()) return false;
          if (n9.equals(2) || n9.equals(3) || n9.equals(5)) return true;
          if (n9.isEven() || n9.isDivisibleBy(3) || n9.isDivisibleBy(5)) return false;
          if (n9.lesser(49)) return true;
        }
        function millerRabinTest(n9, a4) {
          var nPrev = n9.prev(), b2 = nPrev, r6 = 0, d3, t5, i6, x2;
          while (b2.isEven()) b2 = b2.divide(2), r6++;
          next: for (i6 = 0; i6 < a4.length; i6++) {
            if (n9.lesser(a4[i6])) continue;
            x2 = bigInt2(a4[i6]).modPow(b2, n9);
            if (x2.isUnit() || x2.equals(nPrev)) continue;
            for (d3 = r6 - 1; d3 != 0; d3--) {
              x2 = x2.square().mod(n9);
              if (x2.isUnit()) return false;
              if (x2.equals(nPrev)) continue next;
            }
            return false;
          }
          return true;
        }
        BigInteger.prototype.isPrime = function(strict) {
          var isPrime = isBasicPrime(this);
          if (isPrime !== undefined2) return isPrime;
          var n9 = this.abs();
          var bits = n9.bitLength();
          if (bits <= 64)
            return millerRabinTest(n9, [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37]);
          var logN = Math.log(2) * bits.toJSNumber();
          var t5 = Math.ceil(strict === true ? 2 * Math.pow(logN, 2) : logN);
          for (var a4 = [], i6 = 0; i6 < t5; i6++) {
            a4.push(bigInt2(i6 + 2));
          }
          return millerRabinTest(n9, a4);
        };
        NativeBigInt.prototype.isPrime = SmallInteger.prototype.isPrime = BigInteger.prototype.isPrime;
        BigInteger.prototype.isProbablePrime = function(iterations, rng) {
          var isPrime = isBasicPrime(this);
          if (isPrime !== undefined2) return isPrime;
          var n9 = this.abs();
          var t5 = iterations === undefined2 ? 5 : iterations;
          for (var a4 = [], i6 = 0; i6 < t5; i6++) {
            a4.push(bigInt2.randBetween(2, n9.minus(2), rng));
          }
          return millerRabinTest(n9, a4);
        };
        NativeBigInt.prototype.isProbablePrime = SmallInteger.prototype.isProbablePrime = BigInteger.prototype.isProbablePrime;
        BigInteger.prototype.modInv = function(n9) {
          var t5 = bigInt2.zero, newT = bigInt2.one, r6 = parseValue(n9), newR = this.abs(), q, lastT, lastR;
          while (!newR.isZero()) {
            q = r6.divide(newR);
            lastT = t5;
            lastR = r6;
            t5 = newT;
            r6 = newR;
            newT = lastT.subtract(q.multiply(newT));
            newR = lastR.subtract(q.multiply(newR));
          }
          if (!r6.isUnit()) throw new Error(this.toString() + " and " + n9.toString() + " are not co-prime");
          if (t5.compare(0) === -1) {
            t5 = t5.add(n9);
          }
          if (this.isNegative()) {
            return t5.negate();
          }
          return t5;
        };
        NativeBigInt.prototype.modInv = SmallInteger.prototype.modInv = BigInteger.prototype.modInv;
        BigInteger.prototype.next = function() {
          var value = this.value;
          if (this.sign) {
            return subtractSmall(value, 1, this.sign);
          }
          return new BigInteger(addSmall(value, 1), this.sign);
        };
        SmallInteger.prototype.next = function() {
          var value = this.value;
          if (value + 1 < MAX_INT) return new SmallInteger(value + 1);
          return new BigInteger(MAX_INT_ARR, false);
        };
        NativeBigInt.prototype.next = function() {
          return new NativeBigInt(this.value + BigInt(1));
        };
        BigInteger.prototype.prev = function() {
          var value = this.value;
          if (this.sign) {
            return new BigInteger(addSmall(value, 1), true);
          }
          return subtractSmall(value, 1, this.sign);
        };
        SmallInteger.prototype.prev = function() {
          var value = this.value;
          if (value - 1 > -MAX_INT) return new SmallInteger(value - 1);
          return new BigInteger(MAX_INT_ARR, true);
        };
        NativeBigInt.prototype.prev = function() {
          return new NativeBigInt(this.value - BigInt(1));
        };
        var powersOfTwo = [1];
        while (2 * powersOfTwo[powersOfTwo.length - 1] <= BASE) powersOfTwo.push(2 * powersOfTwo[powersOfTwo.length - 1]);
        var powers2Length = powersOfTwo.length, highestPower2 = powersOfTwo[powers2Length - 1];
        function shift_isSmall(n9) {
          return Math.abs(n9) <= BASE;
        }
        BigInteger.prototype.shiftLeft = function(v2) {
          var n9 = parseValue(v2).toJSNumber();
          if (!shift_isSmall(n9)) {
            throw new Error(String(n9) + " is too large for shifting.");
          }
          if (n9 < 0) return this.shiftRight(-n9);
          var result = this;
          if (result.isZero()) return result;
          while (n9 >= powers2Length) {
            result = result.multiply(highestPower2);
            n9 -= powers2Length - 1;
          }
          return result.multiply(powersOfTwo[n9]);
        };
        NativeBigInt.prototype.shiftLeft = SmallInteger.prototype.shiftLeft = BigInteger.prototype.shiftLeft;
        BigInteger.prototype.shiftRight = function(v2) {
          var remQuo;
          var n9 = parseValue(v2).toJSNumber();
          if (!shift_isSmall(n9)) {
            throw new Error(String(n9) + " is too large for shifting.");
          }
          if (n9 < 0) return this.shiftLeft(-n9);
          var result = this;
          while (n9 >= powers2Length) {
            if (result.isZero() || result.isNegative() && result.isUnit()) return result;
            remQuo = divModAny(result, highestPower2);
            result = remQuo[1].isNegative() ? remQuo[0].prev() : remQuo[0];
            n9 -= powers2Length - 1;
          }
          remQuo = divModAny(result, powersOfTwo[n9]);
          return remQuo[1].isNegative() ? remQuo[0].prev() : remQuo[0];
        };
        NativeBigInt.prototype.shiftRight = SmallInteger.prototype.shiftRight = BigInteger.prototype.shiftRight;
        function bitwise(x2, y2, fn) {
          y2 = parseValue(y2);
          var xSign = x2.isNegative(), ySign = y2.isNegative();
          var xRem = xSign ? x2.not() : x2, yRem = ySign ? y2.not() : y2;
          var xDigit = 0, yDigit = 0;
          var xDivMod = null, yDivMod = null;
          var result = [];
          while (!xRem.isZero() || !yRem.isZero()) {
            xDivMod = divModAny(xRem, highestPower2);
            xDigit = xDivMod[1].toJSNumber();
            if (xSign) {
              xDigit = highestPower2 - 1 - xDigit;
            }
            yDivMod = divModAny(yRem, highestPower2);
            yDigit = yDivMod[1].toJSNumber();
            if (ySign) {
              yDigit = highestPower2 - 1 - yDigit;
            }
            xRem = xDivMod[0];
            yRem = yDivMod[0];
            result.push(fn(xDigit, yDigit));
          }
          var sum = fn(xSign ? 1 : 0, ySign ? 1 : 0) !== 0 ? bigInt2(-1) : bigInt2(0);
          for (var i6 = result.length - 1; i6 >= 0; i6 -= 1) {
            sum = sum.multiply(highestPower2).add(bigInt2(result[i6]));
          }
          return sum;
        }
        BigInteger.prototype.not = function() {
          return this.negate().prev();
        };
        NativeBigInt.prototype.not = SmallInteger.prototype.not = BigInteger.prototype.not;
        BigInteger.prototype.and = function(n9) {
          return bitwise(this, n9, function(a4, b2) {
            return a4 & b2;
          });
        };
        NativeBigInt.prototype.and = SmallInteger.prototype.and = BigInteger.prototype.and;
        BigInteger.prototype.or = function(n9) {
          return bitwise(this, n9, function(a4, b2) {
            return a4 | b2;
          });
        };
        NativeBigInt.prototype.or = SmallInteger.prototype.or = BigInteger.prototype.or;
        BigInteger.prototype.xor = function(n9) {
          return bitwise(this, n9, function(a4, b2) {
            return a4 ^ b2;
          });
        };
        NativeBigInt.prototype.xor = SmallInteger.prototype.xor = BigInteger.prototype.xor;
        var LOBMASK_I = 1 << 30, LOBMASK_BI = (BASE & -BASE) * (BASE & -BASE) | LOBMASK_I;
        function roughLOB(n9) {
          var v2 = n9.value, x2 = typeof v2 === "number" ? v2 | LOBMASK_I : typeof v2 === "bigint" ? v2 | BigInt(LOBMASK_I) : v2[0] + v2[1] * BASE | LOBMASK_BI;
          return x2 & -x2;
        }
        function integerLogarithm(value, base) {
          if (base.compareTo(value) <= 0) {
            var tmp = integerLogarithm(value, base.square(base));
            var p11 = tmp.p;
            var e11 = tmp.e;
            var t5 = p11.multiply(base);
            return t5.compareTo(value) <= 0 ? { p: t5, e: e11 * 2 + 1 } : { p: p11, e: e11 * 2 };
          }
          return { p: bigInt2(1), e: 0 };
        }
        BigInteger.prototype.bitLength = function() {
          var n9 = this;
          if (n9.compareTo(bigInt2(0)) < 0) {
            n9 = n9.negate().subtract(bigInt2(1));
          }
          if (n9.compareTo(bigInt2(0)) === 0) {
            return bigInt2(0);
          }
          return bigInt2(integerLogarithm(n9, bigInt2(2)).e).add(bigInt2(1));
        };
        NativeBigInt.prototype.bitLength = SmallInteger.prototype.bitLength = BigInteger.prototype.bitLength;
        function max(a4, b2) {
          a4 = parseValue(a4);
          b2 = parseValue(b2);
          return a4.greater(b2) ? a4 : b2;
        }
        function min(a4, b2) {
          a4 = parseValue(a4);
          b2 = parseValue(b2);
          return a4.lesser(b2) ? a4 : b2;
        }
        function gcd2(a4, b2) {
          a4 = parseValue(a4).abs();
          b2 = parseValue(b2).abs();
          if (a4.equals(b2)) return a4;
          if (a4.isZero()) return b2;
          if (b2.isZero()) return a4;
          var c6 = Integer[1], d3, t5;
          while (a4.isEven() && b2.isEven()) {
            d3 = min(roughLOB(a4), roughLOB(b2));
            a4 = a4.divide(d3);
            b2 = b2.divide(d3);
            c6 = c6.multiply(d3);
          }
          while (a4.isEven()) {
            a4 = a4.divide(roughLOB(a4));
          }
          do {
            while (b2.isEven()) {
              b2 = b2.divide(roughLOB(b2));
            }
            if (a4.greater(b2)) {
              t5 = b2;
              b2 = a4;
              a4 = t5;
            }
            b2 = b2.subtract(a4);
          } while (!b2.isZero());
          return c6.isUnit() ? a4 : a4.multiply(c6);
        }
        function lcm2(a4, b2) {
          a4 = parseValue(a4).abs();
          b2 = parseValue(b2).abs();
          return a4.divide(gcd2(a4, b2)).multiply(b2);
        }
        function randBetween(a4, b2, rng) {
          a4 = parseValue(a4);
          b2 = parseValue(b2);
          var usedRNG = rng || Math.random;
          var low = min(a4, b2), high = max(a4, b2);
          var range = high.subtract(low).add(1);
          if (range.isSmall) return low.add(Math.floor(usedRNG() * range));
          var digits = toBase(range, BASE).value;
          var result = [], restricted = true;
          for (var i6 = 0; i6 < digits.length; i6++) {
            var top2 = restricted ? digits[i6] + (i6 + 1 < digits.length ? digits[i6 + 1] / BASE : 0) : BASE;
            var digit = truncate(usedRNG() * top2);
            result.push(digit);
            if (digit < digits[i6]) restricted = false;
          }
          return low.add(Integer.fromArray(result, BASE, false));
        }
        var parseBase = function(text, base, alphabet, caseSensitive) {
          alphabet = alphabet || DEFAULT_ALPHABET;
          text = String(text);
          if (!caseSensitive) {
            text = text.toLowerCase();
            alphabet = alphabet.toLowerCase();
          }
          var length2 = text.length;
          var i6;
          var absBase = Math.abs(base);
          var alphabetValues = {};
          for (i6 = 0; i6 < alphabet.length; i6++) {
            alphabetValues[alphabet[i6]] = i6;
          }
          for (i6 = 0; i6 < length2; i6++) {
            var c6 = text[i6];
            if (c6 === "-") continue;
            if (c6 in alphabetValues) {
              if (alphabetValues[c6] >= absBase) {
                if (c6 === "1" && absBase === 1) continue;
                throw new Error(c6 + " is not a valid digit in base " + base + ".");
              }
            }
          }
          base = parseValue(base);
          var digits = [];
          var isNegative = text[0] === "-";
          for (i6 = isNegative ? 1 : 0; i6 < text.length; i6++) {
            var c6 = text[i6];
            if (c6 in alphabetValues) digits.push(parseValue(alphabetValues[c6]));
            else if (c6 === "<") {
              var start = i6;
              do {
                i6++;
              } while (text[i6] !== ">" && i6 < text.length);
              digits.push(parseValue(text.slice(start + 1, i6)));
            } else throw new Error(c6 + " is not a valid character");
          }
          return parseBaseFromArray(digits, base, isNegative);
        };
        function parseBaseFromArray(digits, base, isNegative) {
          var val = Integer[0], pow = Integer[1], i6;
          for (i6 = digits.length - 1; i6 >= 0; i6--) {
            val = val.add(digits[i6].times(pow));
            pow = pow.times(base);
          }
          return isNegative ? val.negate() : val;
        }
        function stringify(digit, alphabet) {
          alphabet = alphabet || DEFAULT_ALPHABET;
          if (digit < alphabet.length) {
            return alphabet[digit];
          }
          return "<" + digit + ">";
        }
        function toBase(n9, base) {
          base = bigInt2(base);
          if (base.isZero()) {
            if (n9.isZero()) return { value: [0], isNegative: false };
            throw new Error("Cannot convert nonzero numbers to base 0.");
          }
          if (base.equals(-1)) {
            if (n9.isZero()) return { value: [0], isNegative: false };
            if (n9.isNegative())
              return {
                value: [].concat.apply(
                  [],
                  Array.apply(null, Array(-n9.toJSNumber())).map(Array.prototype.valueOf, [1, 0])
                ),
                isNegative: false
              };
            var arr = Array.apply(null, Array(n9.toJSNumber() - 1)).map(Array.prototype.valueOf, [0, 1]);
            arr.unshift([1]);
            return {
              value: [].concat.apply([], arr),
              isNegative: false
            };
          }
          var neg = false;
          if (n9.isNegative() && base.isPositive()) {
            neg = true;
            n9 = n9.abs();
          }
          if (base.isUnit()) {
            if (n9.isZero()) return { value: [0], isNegative: false };
            return {
              value: Array.apply(null, Array(n9.toJSNumber())).map(Number.prototype.valueOf, 1),
              isNegative: neg
            };
          }
          var out = [];
          var left = n9, divmod;
          while (left.isNegative() || left.compareAbs(base) >= 0) {
            divmod = left.divmod(base);
            left = divmod.quotient;
            var digit = divmod.remainder;
            if (digit.isNegative()) {
              digit = base.minus(digit).abs();
              left = left.next();
            }
            out.push(digit.toJSNumber());
          }
          out.push(left.toJSNumber());
          return { value: out.reverse(), isNegative: neg };
        }
        function toBaseString(n9, base, alphabet) {
          var arr = toBase(n9, base);
          return (arr.isNegative ? "-" : "") + arr.value.map(function(x2) {
            return stringify(x2, alphabet);
          }).join("");
        }
        BigInteger.prototype.toArray = function(radix) {
          return toBase(this, radix);
        };
        SmallInteger.prototype.toArray = function(radix) {
          return toBase(this, radix);
        };
        NativeBigInt.prototype.toArray = function(radix) {
          return toBase(this, radix);
        };
        BigInteger.prototype.toString = function(radix, alphabet) {
          if (radix === undefined2) radix = 10;
          if (radix !== 10 || alphabet) return toBaseString(this, radix, alphabet);
          var v2 = this.value, l8 = v2.length, str = String(v2[--l8]), zeros = "0000000", digit;
          while (--l8 >= 0) {
            digit = String(v2[l8]);
            str += zeros.slice(digit.length) + digit;
          }
          var sign2 = this.sign ? "-" : "";
          return sign2 + str;
        };
        SmallInteger.prototype.toString = function(radix, alphabet) {
          if (radix === undefined2) radix = 10;
          if (radix != 10 || alphabet) return toBaseString(this, radix, alphabet);
          return String(this.value);
        };
        NativeBigInt.prototype.toString = SmallInteger.prototype.toString;
        NativeBigInt.prototype.toJSON = BigInteger.prototype.toJSON = SmallInteger.prototype.toJSON = function() {
          return this.toString();
        };
        BigInteger.prototype.valueOf = function() {
          return parseInt(this.toString(), 10);
        };
        BigInteger.prototype.toJSNumber = BigInteger.prototype.valueOf;
        SmallInteger.prototype.valueOf = function() {
          return this.value;
        };
        SmallInteger.prototype.toJSNumber = SmallInteger.prototype.valueOf;
        NativeBigInt.prototype.valueOf = NativeBigInt.prototype.toJSNumber = function() {
          return parseInt(this.toString(), 10);
        };
        function parseStringValue(v2) {
          if (isPrecise(+v2)) {
            var x2 = +v2;
            if (x2 === truncate(x2))
              return supportsNativeBigInt ? new NativeBigInt(BigInt(x2)) : new SmallInteger(x2);
            throw new Error("Invalid integer: " + v2);
          }
          var sign2 = v2[0] === "-";
          if (sign2) v2 = v2.slice(1);
          var split = v2.split(/e/i);
          if (split.length > 2) throw new Error("Invalid integer: " + split.join("e"));
          if (split.length === 2) {
            var exp = split[1];
            if (exp[0] === "+") exp = exp.slice(1);
            exp = +exp;
            if (exp !== truncate(exp) || !isPrecise(exp)) throw new Error("Invalid integer: " + exp + " is not a valid exponent.");
            var text = split[0];
            var decimalPlace = text.indexOf(".");
            if (decimalPlace >= 0) {
              exp -= text.length - decimalPlace - 1;
              text = text.slice(0, decimalPlace) + text.slice(decimalPlace + 1);
            }
            if (exp < 0) throw new Error("Cannot include negative exponent part for integers");
            text += new Array(exp + 1).join("0");
            v2 = text;
          }
          var isValid = /^([0-9][0-9]*)$/.test(v2);
          if (!isValid) throw new Error("Invalid integer: " + v2);
          if (supportsNativeBigInt) {
            return new NativeBigInt(BigInt(sign2 ? "-" + v2 : v2));
          }
          var r6 = [], max2 = v2.length, l8 = LOG_BASE, min2 = max2 - l8;
          while (max2 > 0) {
            r6.push(+v2.slice(min2, max2));
            min2 -= l8;
            if (min2 < 0) min2 = 0;
            max2 -= l8;
          }
          trim(r6);
          return new BigInteger(r6, sign2);
        }
        function parseNumberValue(v2) {
          if (supportsNativeBigInt) {
            return new NativeBigInt(BigInt(v2));
          }
          if (isPrecise(v2)) {
            if (v2 !== truncate(v2)) throw new Error(v2 + " is not an integer.");
            return new SmallInteger(v2);
          }
          return parseStringValue(v2.toString());
        }
        function parseValue(v2) {
          if (typeof v2 === "number") {
            return parseNumberValue(v2);
          }
          if (typeof v2 === "string") {
            return parseStringValue(v2);
          }
          if (typeof v2 === "bigint") {
            return new NativeBigInt(v2);
          }
          return v2;
        }
        for (var i5 = 0; i5 < 1e3; i5++) {
          Integer[i5] = parseValue(i5);
          if (i5 > 0) Integer[-i5] = parseValue(-i5);
        }
        Integer.one = Integer[1];
        Integer.zero = Integer[0];
        Integer.minusOne = Integer[-1];
        Integer.max = max;
        Integer.min = min;
        Integer.gcd = gcd2;
        Integer.lcm = lcm2;
        Integer.isInstance = function(x2) {
          return x2 instanceof BigInteger || x2 instanceof SmallInteger || x2 instanceof NativeBigInt;
        };
        Integer.randBetween = randBetween;
        Integer.fromArray = function(digits, base, isNegative) {
          return parseBaseFromArray(digits.map(parseValue), parseValue(base || 10), isNegative);
        };
        return Integer;
      }();
      if (typeof module2 !== "undefined" && module2.hasOwnProperty("exports")) {
        module2.exports = bigInt2;
      }
      if (typeof define === "function" && define.amd) {
        define(function() {
          return bigInt2;
        });
      }
    }
  });

  // ../../../../../../../../Dropbox/projects/hazel/node_modules/algebrite/dist/algebrite.js
  var require_algebrite = __commonJS({
    "../../../../../../../../Dropbox/projects/hazel/node_modules/algebrite/dist/algebrite.js"(exports, module) {
      (function() {
        var $, ABS, ADD, ADJ, AND, APPROXRATIO, ARCCOS, ARCCOSH, ARCSIN, ARCSINH, ARCTAN, ARCTANH, ARG, ASSUME_REAL_VARIABLES, ATOMIZE, AUTOEXPAND, BAKE, BESSELJ, BESSELY, BINDING, BINOMIAL, BINOM_check_args, BUF, C1, C2, C3, C4, C5, C6, CEILING, CHECK, CHOOSE, CIRCEXP, CLEAR, CLEARALL, CLEARPATTERNS, CLOCK, COEFF, COFACTOR, CONDENSE, CONJ, CONS, CONTRACT, COS, COSH, Condense, DEBUG, DEBUG_ABS, DEBUG_ARG, DEBUG_CLOCKFORM, DEBUG_IMAG, DEBUG_IS, DEBUG_MULTIPLY, DEBUG_POWER, DEBUG_RATIONALIZE, DEBUG_RECT, DEBUG_SIMPLIFY, DECOMP, DEFINT, DEGREE, DENOMINATOR, DERIVATIVE, DET, DET_check_arg, DIM, DIRAC, DIVISORS, DO, DOT, DOUBLE, DRAW, DRAWX, DSOLVE, E, EIGEN, EIGENVAL, EIGENVEC, EIG_N, EIG_check_arg, EIG_yydd, EIG_yyqq, ERF, ERFC, EVAL, EXP, EXPAND, EXPCOS, EXPSIN, Eval, Eval_Eval, Eval_abs, Eval_add, Eval_adj, Eval_and, Eval_approxratio, Eval_arccos, Eval_arccosh, Eval_arcsin, Eval_arcsinh, Eval_arctan, Eval_arctanh, Eval_arg, Eval_besselj, Eval_bessely, Eval_binding, Eval_binomial, Eval_ceiling, Eval_check, Eval_choose, Eval_circexp, Eval_clear, Eval_clearall, Eval_clearpatterns, Eval_clock, Eval_coeff, Eval_cofactor, Eval_condense, Eval_conj, Eval_cons, Eval_contract, Eval_cos, Eval_cosh, Eval_decomp, Eval_defint, Eval_degree, Eval_denominator, Eval_derivative, Eval_det, Eval_dim, Eval_dirac, Eval_divisors, Eval_do, Eval_dsolve, Eval_eigen, Eval_eigenval, Eval_eigenvec, Eval_erf, Eval_erfc, Eval_exp, Eval_expand, Eval_expcos, Eval_expsin, Eval_factor, Eval_factorial, Eval_factorpoly, Eval_filter, Eval_float, Eval_floor, Eval_for, Eval_function_reference, Eval_gamma, Eval_gcd, Eval_hermite, Eval_hilbert, Eval_imag, Eval_index, Eval_inner, Eval_integral, Eval_inv, Eval_invg, Eval_isinteger, Eval_isprime, Eval_laguerre, Eval_lcm, Eval_leading, Eval_legendre, Eval_log, Eval_lookup, Eval_mod, Eval_multiply, Eval_noexpand, Eval_not, Eval_nroots, Eval_number, Eval_numerator, Eval_operator, Eval_or, Eval_outer, Eval_pattern, Eval_patternsinfo, Eval_polar, Eval_power, Eval_predicate, Eval_prime, Eval_print, Eval_print2dascii, Eval_printcomputer, Eval_printhuman, Eval_printlatex, Eval_printlist, Eval_product, Eval_quote, Eval_quotient, Eval_rank, Eval_rationalize, Eval_real, Eval_rect, Eval_roots, Eval_round, Eval_setq, Eval_sgn, Eval_shape, Eval_silentpattern, Eval_simfac, Eval_simplify, Eval_sin, Eval_sinh, Eval_sqrt, Eval_stop, Eval_subst, Eval_sum, Eval_sym, Eval_symbolsinfo, Eval_tan, Eval_tanh, Eval_taylor, Eval_tensor, Eval_test, Eval_testeq, Eval_testge, Eval_testgt, Eval_testle, Eval_testlt, Eval_transpose, Eval_unit, Eval_user_function, Eval_zero, Evalpoly, FACTOR, FACTORIAL, FACTORPOLY, FILTER, FLOATF, FLOOR, FOR, FORCE_FIXED_PRINTOUT, FUNCTION, Find, GAMMA, GCD, HERMITE, HILBERT, IMAG, INDEX, INNER, INTEGRAL, INV, INVG, INV_check_arg, INV_decomp, ISINTEGER, ISPRIME, LAGUERRE, LAST, LAST_2DASCII_PRINT, LAST_FULL_PRINT, LAST_LATEX_PRINT, LAST_LIST_PRINT, LAST_PLAIN_PRINT, LAST_PRINT, LCM, LEADING, LEGENDRE, LOG, LOOKUP, M, MAXDIM, MAXPRIMETAB, MAX_CONSECUTIVE_APPLICATIONS_OF_ALL_RULES, MAX_CONSECUTIVE_APPLICATIONS_OF_SINGLE_RULE, MAX_FIXED_PRINTOUT_DIGITS, MAX_PROGRAM_SIZE, MEQUAL, METAA, METAB, METAX, MLENGTH, MOD, MSIGN, MULTIPLY, MZERO, N, NIL, NOT, NROOTS, NROOTS_ABS, NROOTS_DELTA, NROOTS_EPSILON, NROOTS_RANDOM, NROOTS_YMAX, NROOTS_divpoly, NSYM, NUM, NUMBER, NUMERATOR, OPERATOR, OR, OUTER, PATTERN, PATTERNSINFO, PI, POLAR, POWER, PRIME, PRINT, PRINT2DASCII, PRINTFULL, PRINTLATEX, PRINTLIST, PRINTMODE_2DASCII, PRINTMODE_COMPUTER, PRINTMODE_HUMAN, PRINTMODE_LATEX, PRINTMODE_LIST, PRINTOUTRESULT, PRINTPLAIN, PRINT_LEAVE_E_ALONE, PRINT_LEAVE_X_ALONE, PRODUCT, QUOTE, QUOTIENT, RANK, RATIONALIZE, REAL, ROOTS, ROUND, SECRETX, SELFTEST, SETQ, SGN, SHAPE, SILENTPATTERN, SIMPLIFY, SIN, SINH, SPACE_BETWEEN_COLUMNS, SPACE_BETWEEN_ROWS, SQRT, STOP, STR, SUBST, SUM, SYM, SYMBOLSINFO, SYMBOL_A, SYMBOL_A_UNDERSCORE, SYMBOL_B, SYMBOL_B_UNDERSCORE, SYMBOL_C, SYMBOL_D, SYMBOL_I, SYMBOL_IDENTITY_MATRIX, SYMBOL_J, SYMBOL_N, SYMBOL_R, SYMBOL_S, SYMBOL_T, SYMBOL_X, SYMBOL_X_UNDERSCORE, SYMBOL_Y, SYMBOL_Z, TAN, TANH, TAYLOR, TENSOR, TEST, TESTEQ, TESTGE, TESTGT, TESTLE, TESTLT, TIMING_DEBUGS, TOS, TRACE, TRANSPOSE, T_DOUBLE, T_EQ, T_FUNCTION, T_GTEQ, T_INTEGER, T_LTEQ, T_NEQ, T_NEWLINE, T_QUOTASSIGN, T_STRING, T_SYMBOL, U, UNIT, USR_SYMBOLS, VERSION, YMAX, YYE, YYRECT, ZERO, __emit_char, __emit_str, __factor_add, __factorial, __is_negative, __is_radical_number, __lcm, __legendre, __legendre2, __legendre3, __normalize_radical_factors, __rationalize_tensor, _print, abs, absValFloat, absval, absval_tensor, add, addSymbolLeftOfAssignment, addSymbolRightOfAssignment, add_all, add_factor_to_accumulator, add_numbers, add_terms, addf, adj, alloc_tensor, allocatedId, any_denominators, approxAll, approxLogs, approxLogsOfRationals, approxOneRatioOnly, approxRadicals, approxRadicalsOfRationals, approxRationalsOfLogs, approxRationalsOfPowersOfE, approxRationalsOfPowersOfPI, approxRationalsOfRadicals, approxSineOfRationalMultiplesOfPI, approxSineOfRationals, approxTrigonometric, approx_just_an_integer, approx_logarithmsOfRationals, approx_nothingUseful, approx_radicalOfRatio, approx_ratioOfRadical, approx_rationalOfE, approx_rationalOfPi, approx_rationalsOfLogarithms, approx_sine_of_pi_times_rational, approx_sine_of_rational, approxratioRecursive, arccos, arccosh, arcsin, arcsinh, arctan, arctanh, areunivarpolysfactoredorexpandedform, arg, arglist, assignmentFound, avoidCalculatingPowersIntoArctans, bake, bake_poly, bake_poly_term, besselj, bessely, bigInt, bignum_factorial, bignum_float, bignum_power_number, bignum_scan_float, bignum_scan_integer, bignum_truncate, binding, binomial, buffer, build_tensor, caaddr, caadr, caar, cadaddr, cadadr, cadar, caddaddr, caddadr, caddar, caddddr, cadddr, caddr, cadr, called_from_Algebra_block, car, cdaddr, cdadr, cdar, cddaddr, cddar, cdddaddr, cddddr, cdddr, cddr, cdr, ceiling, chainOfUserSymbolsNotFunctionsBeingEvaluated, charTabIndex, chartab, checkFloatHasWorkedOutCompletely, check_esc_flag, check_stack, check_tensor_dimensions, choose, choose_check_args, circexp, clearAlgebraEnvironment, clearRenamedVariablesToAvoidBindingToExternalScope, clear_symbols, clear_term, clearall, clockform, cmpGlyphs, cmp_args, cmp_expr, cmp_terms, cmp_terms_count, codeGen, coeff, cofactor, collectLatexStringFromReturnValue, collectUserSymbols, combine_factors, combine_gammas, combine_terms, compareState, compare_numbers, compare_rationals, compare_tensors, compatible, computeDependenciesFromAlgebra, computeResultsAndJavaScriptFromAlgebra, compute_fa, conjugate, cons, consCount, contract, convert_bignum_to_double, convert_rational_to_double, copy_tensor, cosine, cosine_of_angle, cosine_of_angle_sum, count, countOccurrencesOfSymbol, count_denominators, counter, countsize, d_scalar_scalar, d_scalar_scalar_1, d_scalar_tensor, d_tensor_scalar, d_tensor_tensor, dabs, darccos, darccosh, darcsin, darcsinh, darctan, darctanh, dbesselj0, dbesseljn, dbessely0, dbesselyn, dcos, dcosh, dd, decomp, decomp_product, decomp_sum, defineSomeHandyConstants, define_user_function, defn, defn_str, degree, denominator, derf, derfc, derivative, derivative_of_integral, det, determinant, detg, dfunction, dhermite, dirac, display, display_flag, displaychar, divide, divide_numbers, divisors, divisors_onstack, divpoly, dlog, do_clearPatterns, do_clearall, do_simplify_nested_radicals, dontCreateNewRadicalsInDenominatorWhenEvalingMultiplication, dotprod_unicode, doubleToReasonableString, dpow, dpower, dproduct, draw_flag, draw_stop_return, dsgn, dsin, dsinh, dsum, dtan, dtanh, dupl, eigen, elelmIndex, elem, emit_denominator, emit_denominators, emit_expr, emit_factor, emit_factorial_function, emit_flat_tensor, emit_fraction, emit_function, emit_index_function, emit_multiply, emit_number, emit_numerators, emit_numerical_fraction, emit_power, emit_string, emit_subexpr, emit_symbol, emit_tensor, emit_tensor_inner, emit_term, emit_top_expr, emit_unsigned_expr, emit_x, equal, equaln, equalq, erfc, errorMessage, esc_flag, evaluatingAsFloats, evaluatingPolar, exec, expand, expand_get_A, expand_get_AF, expand_get_B, expand_get_C, expand_get_CF, expand_tensor, expanding, expcos, exponential, expr_level, expsin, f1, f10, f2, f3, f4, f5, f9, f_equals_a, factor, factor_a, factor_again, factor_b, factor_number, factor_small_number, factor_term, factorial, factorpoly, factors, fill_buf, filter, filter_main, filter_sum, filter_tensor, findDependenciesInScript, findPossibleClockForm, findPossibleExponentialForm, findroot, fixup_fraction, fixup_power, flag, floatToRatioRoutine, fmt_index, fmt_level, fmt_x, frame, freeze, functionInvokationsScanningStack, gamma, gamma_of_sum, gammaf, gcd, gcd_main, gcd_numbers, gcd_polys, gcd_powers_with_same_base, gcd_product_product, gcd_product_sum, gcd_sum, gcd_sum_product, gcd_sum_sum, gen, getSimpleRoots, getStateHash, get_binding, get_factor_from_complex_root, get_factor_from_real_root, get_innerprod_factors, get_next_token, get_printname, get_size, get_token, getdisplaystr, glyph, gp, guess, hasImaginaryCoeff, hasNegativeRationalExponent, hash_addition, hash_function, hash_multiplication, hash_power, hashcode_values, hashed_itab, hermite, hilbert, i1, imag, imaginaryunit, index_function, init, initNRoots, inited, inner, inner_f, input_str, integral, integral_of_form, integral_of_product, integral_of_sum, inv, inverse, invert_number, invg, isNumberOneOverSomething, isNumericAtom, isNumericAtomOrTensor, isSimpleRoot, isSmall, isSymbolLeftOfAssignment, isSymbolReclaimable, isZeroAtom, isZeroAtomOrTensor, isZeroLikeOrNonZeroLikeOrUndetermined, isZeroTensor, is_denominator, is_factor, is_small_integer, is_square_matrix, is_usr_symbol, isadd, isalnumorunderscore, isalpha, isalphaOrUnderscore, iscomplexnumber, iscomplexnumberdouble, iscons, isdenominator, isdigit, isdouble, iseveninteger, isfactor, isfactorial, isfloating, isfraction, isidentitymatrix, isimaginarynumber, isimaginarynumberdouble, isimaginaryunit, isinnerordot, isinteger, isintegerfactor, isintegerorintegerfloat, isinv, iskeyword, isminusone, isminusoneoversqrttwo, isminusoneovertwo, isminussqrtthreeovertwo, ismultiply, isnegative, isnegativenumber, isnegativeterm, isnonnegativeinteger, isnpi, isone, isoneover, isoneoversqrttwo, isoneovertwo, isplusone, isplustwo, ispolyexpandedform, ispolyexpandedform_expr, ispolyexpandedform_factor, ispolyexpandedform_term, ispolyfactoredorexpandedform, ispolyfactoredorexpandedform_factor, ispolyfactoredorexpandedform_power, isposint, ispositivenumber, ispower, isquarterturn, isrational, isspace, issqrtthree, issqrtthreeovertwo, isstr, issymbol, issymbolic, istensor, istranspose, isunderscore, isunivarpolyfactoredorexpandedform, itab, italu_hashcode, j1, laguerre, laguerre2, lastFoundSymbol, latexErrorSign, lcm, leading, legendre, length, lessp, level, list, listLength, logarithm, logbuf, lookupsTotal, lu_decomp, madd, makePositive, makeSignSameAs, make_hashed_itab, mask, mcmp, mcmpint, mdiv, mdivrem, meta_mode, mgcd, mini_solve, mint, mmod, mmul, mod, monic, move, moveTos, mp_clr_bit, mp_denominator, mp_numerator, mp_set_bit, mpow, mprime, mroot, mshiftright, msub, mtotal, multinomial_sum, multiply, multiply_all, multiply_all_noexpand, multiply_consecutive_constants, multiply_denominators, multiply_denominators_factor, multiply_denominators_term, multiply_noexpand, multiply_numbers, n_factor_number, negate, negate_expand, negate_noexpand, negate_number, new_integer, new_string, newline_flag, nil_symbols, normaliseDots, normalisedCoeff, normalize_angle, nroots_a, nroots_b, nroots_c, nroots_df, nroots_dx, nroots_fa, nroots_fb, nroots_x, nroots_y, nterms, nthCadr, numerator, numericRootOfPolynomial, o, one, oneElement, one_as_double, out_buf, out_count, out_of_memory, outer, p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, parse, parse_internal, parse_p1, parse_p2, parse_time_simplifications, partition, patternHasBeenFound, patternsinfo, performing_roots, polar, polarRectAMinusOneBase, polyform, pop, pop_double, pop_frame, pop_integer, power, power_str, power_sum, power_tensor, predefinedSymbolsInGlobalScope_doNotTrackInDependencies, prime, primetab, print2dascii, printMode, print_ABS_latex, print_ARCCOS_codegen, print_ARCSIN_codegen, print_ARCTAN_codegen, print_BINOMIAL_latex, print_COS_codegen, print_DEFINT_latex, print_DOT_codegen, print_DOT_latex, print_DO_codegen, print_FOR_codegen, print_INV_codegen, print_INV_latex, print_PRODUCT_codegen, print_PRODUCT_latex, print_SETQ_codegen, print_SIN_codegen, print_SQRT_latex, print_SUM_codegen, print_SUM_latex, print_TAN_codegen, print_TESTEQ_latex, print_TESTGE_latex, print_TESTGT_latex, print_TESTLE_latex, print_TESTLT_latex, print_TEST_codegen, print_TEST_latex, print_TRANSPOSE_codegen, print_TRANSPOSE_latex, print_UNIT_codegen, print_a_over_b, print_base, print_base_of_denom, print_char, print_denom, print_double, print_expo_of_denom, print_exponent, print_expr, print_factor, print_factorial_function, print_glyphs, print_index_function, print_list, print_multiply_sign, print_number, print_power, print_str, print_subexpr, print_tensor, print_tensor_inner, print_tensor_inner_latex, print_tensor_latex, print_term, printchar, printchar_nowrap, printline, program_buf, promote_tensor, push, pushTryNotToDuplicate, push_cars, push_double, push_factor, push_frame, push_identity_matrix, push_integer, push_rational, push_symbol, push_term_factors, push_terms, push_zero_matrix, qadd, qdiv, qmul, qpow, qpowf, quickfactor, quickpower, rational, rationalize, rationalize_coefficients, real, reciprocate, rect, recursionLevelNestedRadicalsRemoval, recursiveDependencies, ref, ref1, rememberPrint, remove_negative_exponents, reset_after_error, restore, restoreMetaBindings, rewrite_args, rewrite_args_tensor, roots, roots2, roots3, run, runUserDefinedSimplifications, save, saveMetaBindings, scalar_times_tensor, scan, scan_error, scan_expression, scan_factor, scan_function_call_with_function_name, scan_function_call_without_function_name, scan_index, scan_meta, scan_power, scan_relation, scan_stmt, scan_str, scan_string, scan_subexpr, scan_symbol, scan_tensor, scan_term, scanned, scanningParameters, setM, setSignTo, set_binding, set_component, setq_indexed, sfac_product, sfac_product_f, sgn, shape, show_power_debug, sign, sign_of_term, simfac, simfac_term, simpleComplexityMeasure, simplify, simplifyForCodeGeneration, simplify_1_in_products, simplify_main, simplify_nested_radicals, simplify_polar, simplify_polarRect, simplify_rational_expressions, simplify_rectToClock, simplify_tensor, simplify_trig, simplifyfactorials, sine, sine_of_angle, sine_of_angle_sum, skipRootVariableToBeSolved, sort_stack, square, ssqrt, stack, stackAddsCount, std_symbol, step, step2, stop, strcmp, stringsEmittedByUserPrintouts, subf, subst, subtract, subtract_numbers, swap, symbol, symbolsDependencies, symbolsHavingReassignments, symbolsInExpressionsWithoutAssignments, symbolsLeftOfAssignment, symbolsRightOfAssignment, symbolsinfo, symnum, symtab, take_care_of_nested_radicals, tangent, taylor, tensor, tensor_plus_tensor, tensor_times_scalar, testApprox, test_flag, text_metric, theRandom, token, token_buf, token_str, top, top_level_eval, tos, transform, transpose, transpose_unicode, trigmode, trivial_divide, try_kth_prime, turnErrorMessageToLatex, ucmp, unfreeze, unique, unique_f, update_token_buf, userSimplificationsInListForm, userSimplificationsInStringForm, usr_symbol, verbosing, version, will_be_displayed_as_fraction, ybinomial, ycosh, ydirac, yerf, yerfc, yfloor, yindex, yround, ysinh, yyarg, yybesselj, yybessely, yyceiling, yycondense, yycontract, yycosh, yydegree, yydetg, yydivpoly, yyerf, yyerfc, yyexpand, yyfactorpoly, yyfloat, yyfloor, yyhermite, yyhermite2, yyinvg, yylcm, yylog, yymultiply, yyouter, yypower, yyrationalize, yyround, yysgn, yysimfac, yysinh, yytangent, zero, zzfloat, hasProp = {}.hasOwnProperty;
        bigInt = require_BigInteger();
        version = "1.4.0";
        SELFTEST = 1;
        NSYM = 1e3;
        DEBUG = false;
        PRINTOUTRESULT = false;
        PRINTMODE_LATEX = "PRINTMODE_LATEX";
        PRINTMODE_2DASCII = "PRINTMODE_2DASCII";
        PRINTMODE_COMPUTER = "PRINTMODE_COMPUTER";
        PRINTMODE_HUMAN = "PRINTMODE_HUMAN";
        PRINTMODE_LIST = "PRINTMODE_LIST";
        printMode = PRINTMODE_COMPUTER;
        dontCreateNewRadicalsInDenominatorWhenEvalingMultiplication = true;
        recursionLevelNestedRadicalsRemoval = 0;
        do_simplify_nested_radicals = true;
        avoidCalculatingPowersIntoArctans = true;
        rational = function() {
          class rational2 {
          }
          ;
          rational2.prototype.a = null;
          rational2.prototype.b = null;
          return rational2;
        }.call(this);
        U = function() {
          class U2 {
            toString() {
              return print_expr(this);
            }
            toLatexString() {
              return collectLatexStringFromReturnValue(this);
            }
            constructor() {
              this.cons = {};
              this.cons.car = null;
              this.cons.cdr = null;
              this.q = new rational();
            }
          }
          ;
          U2.prototype.cons = null;
          U2.prototype.printname = "";
          U2.prototype.str = "";
          U2.prototype.tensor = null;
          U2.prototype.q = null;
          U2.prototype.d = 0;
          U2.prototype.k = 0;
          U2.prototype.tag = 0;
          return U2;
        }.call(this);
        errorMessage = "";
        CONS = 0;
        NUM = 1;
        DOUBLE = 2;
        STR = 3;
        TENSOR = 4;
        SYM = 5;
        counter = 0;
        ABS = counter++;
        ADD = counter++;
        ADJ = counter++;
        AND = counter++;
        APPROXRATIO = counter++;
        ARCCOS = counter++;
        ARCCOSH = counter++;
        ARCSIN = counter++;
        ARCSINH = counter++;
        ARCTAN = counter++;
        ARCTANH = counter++;
        ARG = counter++;
        ATOMIZE = counter++;
        BESSELJ = counter++;
        BESSELY = counter++;
        BINDING = counter++;
        BINOMIAL = counter++;
        CEILING = counter++;
        CHECK = counter++;
        CHOOSE = counter++;
        CIRCEXP = counter++;
        CLEAR = counter++;
        CLEARALL = counter++;
        CLEARPATTERNS = counter++;
        CLOCK = counter++;
        COEFF = counter++;
        COFACTOR = counter++;
        CONDENSE = counter++;
        CONJ = counter++;
        CONTRACT = counter++;
        COS = counter++;
        COSH = counter++;
        DECOMP = counter++;
        DEFINT = counter++;
        DEGREE = counter++;
        DENOMINATOR = counter++;
        DERIVATIVE = counter++;
        DET = counter++;
        DIM = counter++;
        DIRAC = counter++;
        DIVISORS = counter++;
        DO = counter++;
        DOT = counter++;
        DRAW = counter++;
        DSOLVE = counter++;
        EIGEN = counter++;
        EIGENVAL = counter++;
        EIGENVEC = counter++;
        ERF = counter++;
        ERFC = counter++;
        EVAL = counter++;
        EXP = counter++;
        EXPAND = counter++;
        EXPCOS = counter++;
        EXPSIN = counter++;
        FACTOR = counter++;
        FACTORIAL = counter++;
        FACTORPOLY = counter++;
        FILTER = counter++;
        FLOATF = counter++;
        FLOOR = counter++;
        FOR = counter++;
        FUNCTION = counter++;
        GAMMA = counter++;
        GCD = counter++;
        HERMITE = counter++;
        HILBERT = counter++;
        IMAG = counter++;
        INDEX = counter++;
        INNER = counter++;
        INTEGRAL = counter++;
        INV = counter++;
        INVG = counter++;
        ISINTEGER = counter++;
        ISPRIME = counter++;
        LAGUERRE = counter++;
        LCM = counter++;
        LEADING = counter++;
        LEGENDRE = counter++;
        LOG = counter++;
        LOOKUP = counter++;
        MOD = counter++;
        MULTIPLY = counter++;
        NOT = counter++;
        NROOTS = counter++;
        NUMBER = counter++;
        NUMERATOR = counter++;
        OPERATOR = counter++;
        OR = counter++;
        OUTER = counter++;
        PATTERN = counter++;
        PATTERNSINFO = counter++;
        POLAR = counter++;
        POWER = counter++;
        PRIME = counter++;
        PRINT_LEAVE_E_ALONE = counter++;
        PRINT_LEAVE_X_ALONE = counter++;
        PRINT = counter++;
        PRINT2DASCII = counter++;
        PRINTFULL = counter++;
        PRINTLATEX = counter++;
        PRINTLIST = counter++;
        PRINTPLAIN = counter++;
        PRODUCT = counter++;
        QUOTE = counter++;
        QUOTIENT = counter++;
        RANK = counter++;
        RATIONALIZE = counter++;
        REAL = counter++;
        ROUND = counter++;
        YYRECT = counter++;
        ROOTS = counter++;
        SETQ = counter++;
        SGN = counter++;
        SILENTPATTERN = counter++;
        SIMPLIFY = counter++;
        SIN = counter++;
        SINH = counter++;
        SHAPE = counter++;
        SQRT = counter++;
        STOP = counter++;
        SUBST = counter++;
        SUM = counter++;
        SYMBOLSINFO = counter++;
        TAN = counter++;
        TANH = counter++;
        TAYLOR = counter++;
        TEST = counter++;
        TESTEQ = counter++;
        TESTGE = counter++;
        TESTGT = counter++;
        TESTLE = counter++;
        TESTLT = counter++;
        TRANSPOSE = counter++;
        UNIT = counter++;
        ZERO = counter++;
        NIL = counter++;
        LAST = counter++;
        LAST_PRINT = counter++;
        LAST_2DASCII_PRINT = counter++;
        LAST_FULL_PRINT = counter++;
        LAST_LATEX_PRINT = counter++;
        LAST_LIST_PRINT = counter++;
        LAST_PLAIN_PRINT = counter++;
        AUTOEXPAND = counter++;
        BAKE = counter++;
        ASSUME_REAL_VARIABLES = counter++;
        TRACE = counter++;
        FORCE_FIXED_PRINTOUT = counter++;
        MAX_FIXED_PRINTOUT_DIGITS = counter++;
        YYE = counter++;
        DRAWX = counter++;
        METAA = counter++;
        METAB = counter++;
        METAX = counter++;
        SECRETX = counter++;
        VERSION = counter++;
        PI = counter++;
        SYMBOL_A = counter++;
        SYMBOL_B = counter++;
        SYMBOL_C = counter++;
        SYMBOL_D = counter++;
        SYMBOL_I = counter++;
        SYMBOL_J = counter++;
        SYMBOL_N = counter++;
        SYMBOL_R = counter++;
        SYMBOL_S = counter++;
        SYMBOL_T = counter++;
        SYMBOL_X = counter++;
        SYMBOL_Y = counter++;
        SYMBOL_Z = counter++;
        SYMBOL_IDENTITY_MATRIX = counter++;
        SYMBOL_A_UNDERSCORE = counter++;
        SYMBOL_B_UNDERSCORE = counter++;
        SYMBOL_X_UNDERSCORE = counter++;
        C1 = counter++;
        C2 = counter++;
        C3 = counter++;
        C4 = counter++;
        C5 = counter++;
        C6 = counter++;
        USR_SYMBOLS = counter++;
        E = YYE;
        TOS = 1e5;
        BUF = 1e4;
        MAX_PROGRAM_SIZE = 100001;
        MAXPRIMETAB = 1e4;
        MAX_CONSECUTIVE_APPLICATIONS_OF_ALL_RULES = 5;
        MAX_CONSECUTIVE_APPLICATIONS_OF_SINGLE_RULE = 10;
        MAXDIM = 24;
        symbolsDependencies = {};
        symbolsHavingReassignments = [];
        symbolsInExpressionsWithoutAssignments = [];
        patternHasBeenFound = false;
        predefinedSymbolsInGlobalScope_doNotTrackInDependencies = ["rationalize", "abs", "e", "i", "pi", "sin", "ceiling", "cos", "roots", "integral", "derivative", "defint", "sqrt", "eig", "cov", "deig", "dcov", "float", "floor", "product", "root", "round", "sum", "test", "unit"];
        parse_time_simplifications = true;
        chainOfUserSymbolsNotFunctionsBeingEvaluated = [];
        stringsEmittedByUserPrintouts = "";
        called_from_Algebra_block = false;
        tensor = function() {
          class tensor2 {
            constructor() {
              this.dim = function() {
                var o12, ref2, results;
                results = [];
                for (o12 = 0, ref2 = MAXDIM; 0 <= ref2 ? o12 <= ref2 : o12 >= ref2; 0 <= ref2 ? o12++ : o12--) {
                  results.push(0);
                }
                return results;
              }();
              this.elem = [];
            }
          }
          ;
          tensor2.prototype.ndim = 0;
          tensor2.prototype.dim = null;
          tensor2.prototype.nelem = 0;
          tensor2.prototype.elem = null;
          return tensor2;
        }.call(this);
        display = function() {
          class display2 {
          }
          ;
          display2.prototype.h = 0;
          display2.prototype.w = 0;
          display2.prototype.n = 0;
          display2.prototype.a = [];
          return display2;
        }.call(this);
        text_metric = function() {
          class text_metric2 {
          }
          ;
          text_metric2.prototype.ascent = 0;
          text_metric2.prototype.descent = 0;
          text_metric2.prototype.width = 0;
          return text_metric2;
        }.call(this);
        tos = 0;
        expanding = 0;
        evaluatingAsFloats = 0;
        evaluatingPolar = 0;
        fmt_x = 0;
        fmt_index = 0;
        fmt_level = 0;
        verbosing = 0;
        primetab = function() {
          var ceil, i5, j2, primes;
          primes = [2];
          i5 = 3;
          while (primes.length < MAXPRIMETAB) {
            j2 = 0;
            ceil = Math.sqrt(i5);
            while (j2 < primes.length && primes[j2] <= ceil) {
              if (i5 % primes[j2] === 0) {
                j2 = -1;
                break;
              }
              j2++;
            }
            if (j2 !== -1) {
              primes.push(i5);
            }
            i5 += 2;
          }
          primes[MAXPRIMETAB] = 0;
          return primes;
        }();
        esc_flag = 0;
        draw_flag = 0;
        mtotal = 0;
        trigmode = 0;
        logbuf = "";
        program_buf = "";
        symtab = [];
        binding = [];
        isSymbolReclaimable = [];
        arglist = [];
        stack = [];
        frame = 0;
        p0 = null;
        p1 = null;
        p2 = null;
        p3 = null;
        p4 = null;
        p5 = null;
        p6 = null;
        p7 = null;
        p8 = null;
        p9 = null;
        zero = null;
        one = null;
        one_as_double = null;
        imaginaryunit = null;
        out_buf = "";
        out_count = 0;
        test_flag = 0;
        codeGen = false;
        draw_stop_return = null;
        userSimplificationsInListForm = [];
        userSimplificationsInStringForm = [];
        transpose_unicode = 7488;
        dotprod_unicode = 183;
        symbol = function(x2) {
          return symtab[x2];
        };
        iscons = function(p11) {
          return p11.k === CONS;
        };
        isrational = function(p11) {
          return p11.k === NUM;
        };
        isdouble = function(p11) {
          return p11.k === DOUBLE;
        };
        isNumericAtom = function(p11) {
          return isrational(p11) || isdouble(p11);
        };
        isstr = function(p11) {
          return p11.k === STR;
        };
        istensor = function(p11) {
          if (p11 == null) {
            debugger;
          } else {
            return p11.k === TENSOR;
          }
        };
        isNumericAtomOrTensor = function(p11) {
          var a4, i5, n9, o12, ref2;
          if (isNumericAtom(p11) || p11 === symbol(SYMBOL_IDENTITY_MATRIX)) {
            return 1;
          }
          if (!istensor(p11) && !isNumericAtom(p11)) {
            return 0;
          }
          n9 = p11.tensor.nelem;
          a4 = p11.tensor.elem;
          for (i5 = o12 = 0, ref2 = n9; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            if (!isNumericAtomOrTensor(a4[i5])) {
              return 0;
            }
          }
          return 1;
        };
        issymbol = function(p11) {
          return p11.k === SYM;
        };
        iskeyword = function(p11) {
          return issymbol(p11) && symnum(p11) < NIL;
        };
        car = function(p11) {
          if (iscons(p11)) {
            return p11.cons.car;
          } else {
            return symbol(NIL);
          }
        };
        cdr = function(p11) {
          if (iscons(p11)) {
            return p11.cons.cdr;
          } else {
            return symbol(NIL);
          }
        };
        caar = function(p11) {
          return car(car(p11));
        };
        cadr = function(p11) {
          return car(cdr(p11));
        };
        cdar = function(p11) {
          return cdr(car(p11));
        };
        cddr = function(p11) {
          return cdr(cdr(p11));
        };
        caadr = function(p11) {
          return car(car(cdr(p11)));
        };
        caddr = function(p11) {
          return car(cdr(cdr(p11)));
        };
        cadar = function(p11) {
          return car(cdr(car(p11)));
        };
        cdadr = function(p11) {
          return cdr(car(cdr(p11)));
        };
        cddar = function(p11) {
          return cdr(cdr(car(p11)));
        };
        cdddr = function(p11) {
          return cdr(cdr(cdr(p11)));
        };
        caaddr = function(p11) {
          return car(car(cdr(cdr(p11))));
        };
        cadadr = function(p11) {
          return car(cdr(car(cdr(p11))));
        };
        caddar = function(p11) {
          return car(cdr(cdr(car(p11))));
        };
        cdaddr = function(p11) {
          return cdr(car(cdr(cdr(p11))));
        };
        cadddr = function(p11) {
          return car(cdr(cdr(cdr(p11))));
        };
        cddddr = function(p11) {
          return cdr(cdr(cdr(cdr(p11))));
        };
        caddddr = function(p11) {
          return car(cdr(cdr(cdr(cdr(p11)))));
        };
        cadaddr = function(p11) {
          return car(cdr(car(cdr(cdr(p11)))));
        };
        cddaddr = function(p11) {
          return cdr(cdr(car(cdr(cdr(p11)))));
        };
        caddadr = function(p11) {
          return car(cdr(cdr(car(cdr(p11)))));
        };
        cdddaddr = function(p11) {
          return cdr(cdr(cdr(car(cdr(cdr(p11))))));
        };
        caddaddr = function(p11) {
          return car(cdr(cdr(car(cdr(cdr(p11))))));
        };
        listLength = function(p11) {
          var startCount;
          startCount = -1;
          while (iscons(p11)) {
            p11 = cdr(p11);
            startCount++;
          }
          return startCount;
        };
        nthCadr = function(p11, n9) {
          var startCount;
          startCount = 0;
          while (startCount <= n9) {
            p11 = cdr(p11);
            startCount++;
          }
          return car(p11);
        };
        isadd = function(p11) {
          return car(p11) === symbol(ADD);
        };
        ismultiply = function(p11) {
          return car(p11) === symbol(MULTIPLY);
        };
        ispower = function(p11) {
          return car(p11) === symbol(POWER);
        };
        isfactorial = function(p11) {
          return car(p11) === symbol(FACTORIAL);
        };
        isinnerordot = function(p11) {
          return car(p11) === symbol(INNER) || car(p11) === symbol(DOT);
        };
        istranspose = function(p11) {
          return car(p11) === symbol(TRANSPOSE);
        };
        isinv = function(p11) {
          return car(p11) === symbol(INV);
        };
        isidentitymatrix = function(p11) {
          return p11 === symbol(SYMBOL_IDENTITY_MATRIX);
        };
        MSIGN = function(p11) {
          if (p11.isPositive()) {
            return 1;
          } else if (p11.isZero()) {
            return 0;
          } else {
            return -1;
          }
        };
        MLENGTH = function(p11) {
          return p11.toString().length;
        };
        MZERO = function(p11) {
          return p11.isZero();
        };
        MEQUAL = function(p11, n9) {
          if (p11 == null) {
            debugger;
          }
          return p11.equals(n9);
        };
        reset_after_error = function() {
          moveTos(0);
          esc_flag = 0;
          draw_flag = 0;
          frame = TOS;
          evaluatingAsFloats = 0;
          return evaluatingPolar = 0;
        };
        $ = typeof exports !== "undefined" && exports !== null ? exports : this;
        $.version = version;
        $.isadd = isadd;
        $.ismultiply = ismultiply;
        $.ispower = ispower;
        $.isfactorial = isfactorial;
        $.car = car;
        $.cdr = cdr;
        $.caar = caar;
        $.cadr = cadr;
        $.cdar = cdar;
        $.cddr = cddr;
        $.caadr = caadr;
        $.caddr = caddr;
        $.cadar = cadar;
        $.cdadr = cdadr;
        $.cddar = cddar;
        $.cdddr = cdddr;
        $.caaddr = caaddr;
        $.cadadr = cadadr;
        $.caddar = caddar;
        $.cdaddr = cdaddr;
        $.cadddr = cadddr;
        $.cddddr = cddddr;
        $.caddddr = caddddr;
        $.cadaddr = cadaddr;
        $.cddaddr = cddaddr;
        $.caddadr = caddadr;
        $.cdddaddr = cdddaddr;
        $.caddaddr = caddaddr;
        $.symbol = symbol;
        $.iscons = iscons;
        $.isrational = isrational;
        $.isdouble = isdouble;
        $.isNumericAtom = isNumericAtom;
        $.isstr = isstr;
        $.istensor = istensor;
        $.issymbol = issymbol;
        $.iskeyword = iskeyword;
        $.CONS = CONS;
        $.NUM = NUM;
        $.DOUBLE = DOUBLE;
        $.STR = STR;
        $.TENSOR = TENSOR;
        $.SYM = SYM;
        DEBUG_ABS = false;
        Eval_abs = function() {
          push(cadr(p1));
          Eval();
          return abs();
        };
        absValFloat = function() {
          Eval();
          absval();
          Eval();
          return zzfloat();
        };
        abs = function() {
          var theArgument2;
          theArgument2 = top();
          if (DEBUG_ABS) {
            console.trace(">>>>  ABS of " + theArgument2);
          }
          numerator();
          if (DEBUG_ABS) {
            console.log("ABS numerator " + stack[tos - 1]);
          }
          absval();
          if (DEBUG_ABS) {
            console.log("ABSVAL numerator: " + stack[tos - 1]);
          }
          push(theArgument2);
          denominator();
          if (DEBUG_ABS) {
            console.log("ABS denominator: " + stack[tos - 1]);
          }
          absval();
          if (DEBUG_ABS) {
            console.log("ABSVAL denominator: " + stack[tos - 1]);
          }
          divide();
          if (DEBUG_ABS) {
            console.log("ABSVAL divided: " + stack[tos - 1]);
          }
          if (DEBUG_ABS) {
            return console.log("<<<<<<<  ABS");
          }
        };
        absval = function() {
          var anyFactorsYet, input;
          save();
          p1 = pop();
          input = p1;
          if (DEBUG_ABS) {
            console.log("ABS of " + p1);
          }
          if (isZeroAtomOrTensor(p1)) {
            if (DEBUG_ABS) {
              console.log(" abs: " + p1 + " just zero");
            }
            push(zero);
            if (DEBUG_ABS) {
              console.log(" --> ABS of " + input + " : " + stack[tos - 1]);
            }
            restore();
            return;
          }
          if (isnegativenumber(p1)) {
            if (DEBUG_ABS) {
              console.log(" abs: " + p1 + " just a negative");
            }
            push(p1);
            negate();
            restore();
            return;
          }
          if (ispositivenumber(p1)) {
            if (DEBUG_ABS) {
              console.log(" abs: " + p1 + " just a positive");
            }
            push(p1);
            if (DEBUG_ABS) {
              console.log(" --> ABS of " + input + " : " + stack[tos - 1]);
            }
            restore();
            return;
          }
          if (p1 === symbol(PI)) {
            if (DEBUG_ABS) {
              console.log(" abs: " + p1 + " of PI");
            }
            push(p1);
            if (DEBUG_ABS) {
              console.log(" --> ABS of " + input + " : " + stack[tos - 1]);
            }
            restore();
            return;
          }
          if (car(p1) === symbol(ADD) && (findPossibleClockForm(p1) || findPossibleExponentialForm(p1) || Find(p1, imaginaryunit))) {
            if (DEBUG_ABS) {
              console.log(" abs: " + p1 + " is a sum");
            }
            if (DEBUG_ABS) {
              console.log("abs of a sum");
            }
            push(p1);
            rect();
            p1 = pop();
            push(p1);
            real();
            push_integer(2);
            power();
            push(p1);
            imag();
            push_integer(2);
            power();
            add();
            push_rational(1, 2);
            power();
            simplify_trig();
            if (DEBUG_ABS) {
              console.log(" --> ABS of " + input + " : " + stack[tos - 1]);
            }
            restore();
            return;
          }
          if (car(p1) === symbol(POWER) && equaln(cadr(p1), -1)) {
            if (DEBUG_ABS) {
              console.log(" abs: " + p1 + " is -1 to any power");
            }
            if (evaluatingAsFloats) {
              if (DEBUG_ABS) {
                console.log(" abs: numeric, so result is 1.0");
              }
              push_double(1);
            } else {
              if (DEBUG_ABS) {
                console.log(" abs: symbolic, so result is 1");
              }
              push_integer(1);
            }
            if (DEBUG_ABS) {
              console.log(" --> ABS of " + input + " : " + stack[tos - 1]);
            }
            restore();
            return;
          }
          if (car(p1) === symbol(POWER) && ispositivenumber(caddr(p1))) {
            if (DEBUG_ABS) {
              console.log(" abs: " + p1 + " is something to the power of a positive number");
            }
            push(cadr(p1));
            abs();
            push(caddr(p1));
            power();
            if (DEBUG_ABS) {
              console.log(" --> ABS of " + input + " : " + stack[tos - 1]);
            }
            restore();
            return;
          }
          if (car(p1) === symbol(POWER) && cadr(p1) === symbol(E)) {
            if (DEBUG_ABS) {
              console.log(" abs: " + p1 + " is an exponential");
            }
            push(caddr(p1));
            real();
            exponential();
            if (DEBUG_ABS) {
              console.log(" --> ABS of " + input + " : " + stack[tos - 1]);
            }
            restore();
            return;
          }
          if (car(p1) === symbol(MULTIPLY)) {
            if (DEBUG_ABS) {
              console.log(" abs: " + p1 + " is a product");
            }
            anyFactorsYet = false;
            p1 = cdr(p1);
            while (iscons(p1)) {
              push(car(p1));
              absval();
              if (anyFactorsYet) {
                multiply();
              }
              anyFactorsYet = true;
              p1 = cdr(p1);
            }
            if (DEBUG_ABS) {
              console.log(" --> ABS of " + input + " : " + stack[tos - 1]);
            }
            restore();
            return;
          }
          if (car(p1) === symbol(ABS)) {
            if (DEBUG_ABS) {
              console.log(" abs: " + p1 + " is abs of a abs");
            }
            push_symbol(ABS);
            push(cadr(p1));
            list(2);
            if (DEBUG_ABS) {
              console.log(" --> ABS of " + input + " : " + stack[tos - 1]);
            }
            restore();
            return;
          }
          if (istensor(p1)) {
            absval_tensor();
            restore();
            return;
          }
          if (isnegativeterm(p1) || car(p1) === symbol(ADD) && isnegativeterm(cadr(p1))) {
            push(p1);
            negate();
            p1 = pop();
          }
          if (DEBUG_ABS) {
            console.log(" abs: " + p1 + " is nothing decomposable");
          }
          push_symbol(ABS);
          push(p1);
          list(2);
          if (DEBUG_ABS) {
            console.log(" --> ABS of " + input + " : " + stack[tos - 1]);
          }
          return restore();
        };
        absval_tensor = function() {
          if (p1.tensor.ndim !== 1) {
            stop("abs(tensor) with tensor rank > 1");
          }
          push(p1);
          push(p1);
          conjugate();
          inner();
          push_rational(1, 2);
          power();
          simplify();
          return Eval();
        };
        flag = 0;
        Eval_add = function() {
          var h5;
          h5 = tos;
          p1 = cdr(p1);
          while (iscons(p1)) {
            push(car(p1));
            Eval();
            p2 = pop();
            push_terms(p2);
            p1 = cdr(p1);
          }
          return add_terms(tos - h5);
        };
        stackAddsCount = 0;
        add_terms = function(n9) {
          var h5, i5, i12, j12, o12, ref2, ref12, results, s7, subsetOfStack;
          stackAddsCount++;
          i5 = 0;
          h5 = tos - n9;
          s7 = h5;
          if (DEBUG) {
            console.log("stack before adding terms #" + stackAddsCount);
          }
          if (DEBUG) {
            for (i5 = o12 = 0, ref2 = tos; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
              console.log(print_list(stack[i5]));
            }
          }
          for (i5 = i12 = 0; i12 < 10; i5 = ++i12) {
            if (n9 < 2) {
              break;
            }
            flag = 0;
            subsetOfStack = stack.slice(h5, h5 + n9);
            subsetOfStack.sort(cmp_terms);
            stack = stack.slice(0, h5).concat(subsetOfStack).concat(stack.slice(h5 + n9));
            if (flag === 0) {
              break;
            }
            n9 = combine_terms(h5, n9);
          }
          moveTos(h5 + n9);
          switch (n9) {
            case 0:
              if (evaluatingAsFloats) {
                push_double(0);
              } else {
                push(zero);
              }
              break;
            case 1:
              break;
            default:
              list(n9);
              p1 = pop();
              push_symbol(ADD);
              push(p1);
              cons();
          }
          if (DEBUG) {
            console.log("stack after adding terms #" + stackAddsCount);
          }
          if (DEBUG) {
            results = [];
            for (i5 = j12 = 0, ref12 = tos; 0 <= ref12 ? j12 < ref12 : j12 > ref12; i5 = 0 <= ref12 ? ++j12 : --j12) {
              results.push(console.log(print_list(stack[i5])));
            }
            return results;
          }
        };
        cmp_terms_count = 0;
        cmp_terms = function(p12, p22) {
          var i5, o12, ref2, t5;
          cmp_terms_count++;
          i5 = 0;
          if (isNumericAtom(p12) && isNumericAtom(p22)) {
            flag = 1;
            return 0;
          }
          if (istensor(p12) && istensor(p22)) {
            if (p12.tensor.ndim < p22.tensor.ndim) {
              return -1;
            }
            if (p12.tensor.ndim > p22.tensor.ndim) {
              return 1;
            }
            for (i5 = o12 = 0, ref2 = p12.tensor.ndim; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
              if (p12.tensor.dim[i5] < p22.tensor.dim[i5]) {
                return -1;
              }
              if (p12.tensor.dim[i5] > p22.tensor.dim[i5]) {
                return 1;
              }
            }
            flag = 1;
            return 0;
          }
          if (car(p12) === symbol(MULTIPLY)) {
            p12 = cdr(p12);
            if (isNumericAtom(car(p12))) {
              p12 = cdr(p12);
              if (cdr(p12) === symbol(NIL)) {
                p12 = car(p12);
              }
            }
          }
          if (car(p22) === symbol(MULTIPLY)) {
            p22 = cdr(p22);
            if (isNumericAtom(car(p22))) {
              p22 = cdr(p22);
              if (cdr(p22) === symbol(NIL)) {
                p22 = car(p22);
              }
            }
          }
          t5 = cmp_expr(p12, p22);
          if (t5 === 0) {
            flag = 1;
          }
          return t5;
        };
        combine_terms = function(s7, n9) {
          var i5, i12, j2, j12, l1, m1, o12, ref2, ref12, ref22, ref3, ref4, ref5, ref6, ref7, ref8, ref9, t5;
          i5 = 0;
          while (i5 < n9 - 1) {
            check_esc_flag();
            p3 = stack[s7 + i5];
            p4 = stack[s7 + i5 + 1];
            if (istensor(p3) && istensor(p4)) {
              push(p3);
              push(p4);
              tensor_plus_tensor();
              p1 = pop();
              if (p1 !== symbol(NIL)) {
                stack[s7 + i5] = p1;
                for (j2 = o12 = ref2 = i5 + 1, ref12 = n9 - 1; ref2 <= ref12 ? o12 < ref12 : o12 > ref12; j2 = ref2 <= ref12 ? ++o12 : --o12) {
                  stack[s7 + j2] = stack[s7 + j2 + 1];
                }
                n9--;
                i5--;
              }
              i5++;
              continue;
            }
            if (istensor(p3) || istensor(p4)) {
              i5++;
              continue;
            }
            if (isNumericAtom(p3) && isNumericAtom(p4)) {
              push(p3);
              push(p4);
              add_numbers();
              p1 = pop();
              if (isZeroAtomOrTensor(p1)) {
                for (j2 = i12 = ref22 = i5, ref3 = n9 - 2; ref22 <= ref3 ? i12 < ref3 : i12 > ref3; j2 = ref22 <= ref3 ? ++i12 : --i12) {
                  stack[s7 + j2] = stack[s7 + j2 + 2];
                }
                n9 -= 2;
              } else {
                stack[s7 + i5] = p1;
                for (j2 = j12 = ref4 = i5 + 1, ref5 = n9 - 1; ref4 <= ref5 ? j12 < ref5 : j12 > ref5; j2 = ref4 <= ref5 ? ++j12 : --j12) {
                  stack[s7 + j2] = stack[s7 + j2 + 1];
                }
                n9--;
              }
              i5--;
              i5++;
              continue;
            }
            if (isNumericAtom(p3) || isNumericAtom(p4)) {
              i5++;
              continue;
            }
            if (evaluatingAsFloats) {
              p1 = one_as_double;
              p2 = one_as_double;
            } else {
              p1 = one;
              p2 = one;
            }
            t5 = 0;
            if (car(p3) === symbol(MULTIPLY)) {
              p3 = cdr(p3);
              t5 = 1;
              if (isNumericAtom(car(p3))) {
                p1 = car(p3);
                p3 = cdr(p3);
                if (cdr(p3) === symbol(NIL)) {
                  p3 = car(p3);
                  t5 = 0;
                }
              }
            }
            if (car(p4) === symbol(MULTIPLY)) {
              p4 = cdr(p4);
              if (isNumericAtom(car(p4))) {
                p2 = car(p4);
                p4 = cdr(p4);
                if (cdr(p4) === symbol(NIL)) {
                  p4 = car(p4);
                }
              }
            }
            if (!equal(p3, p4)) {
              i5++;
              continue;
            }
            push(p1);
            push(p2);
            add_numbers();
            p1 = pop();
            if (isZeroAtomOrTensor(p1)) {
              for (j2 = l1 = ref6 = i5, ref7 = n9 - 2; ref6 <= ref7 ? l1 < ref7 : l1 > ref7; j2 = ref6 <= ref7 ? ++l1 : --l1) {
                stack[s7 + j2] = stack[s7 + j2 + 2];
              }
              n9 -= 2;
              i5--;
              i5++;
              continue;
            }
            push(p1);
            if (t5) {
              push(symbol(MULTIPLY));
              push(p3);
              cons();
            } else {
              push(p3);
            }
            multiply();
            stack[s7 + i5] = pop();
            for (j2 = m1 = ref8 = i5 + 1, ref9 = n9 - 1; ref8 <= ref9 ? m1 < ref9 : m1 > ref9; j2 = ref8 <= ref9 ? ++m1 : --m1) {
              stack[s7 + j2] = stack[s7 + j2 + 1];
            }
            n9--;
            i5--;
            i5++;
          }
          return n9;
        };
        push_terms = function(p11) {
          var results;
          if (car(p11) === symbol(ADD)) {
            p11 = cdr(p11);
            results = [];
            while (iscons(p11)) {
              push(car(p11));
              results.push(p11 = cdr(p11));
            }
            return results;
          } else if (!isZeroAtom(p11)) {
            return push(p11);
          }
        };
        add = function() {
          var h5;
          save();
          p2 = pop();
          p1 = pop();
          h5 = tos;
          push_terms(p1);
          push_terms(p2);
          add_terms(tos - h5);
          return restore();
        };
        add_all = function(k2) {
          var h5, i5, o12, ref2, s7;
          i5 = 0;
          save();
          s7 = tos - k2;
          h5 = tos;
          for (i5 = o12 = 0, ref2 = k2; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            push_terms(stack[s7 + i5]);
          }
          add_terms(tos - h5);
          p1 = pop();
          moveTos(tos - k2);
          push(p1);
          return restore();
        };
        subtract = function() {
          negate();
          return add();
        };
        Eval_adj = function() {
          push(cadr(p1));
          Eval();
          return adj();
        };
        adj = function() {
          var doNothing, i5, i12, j2, n9, o12, ref2, ref12;
          i5 = 0;
          j2 = 0;
          n9 = 0;
          save();
          p1 = pop();
          if (istensor(p1) && p1.tensor.ndim === 2 && p1.tensor.dim[0] === p1.tensor.dim[1]) {
            doNothing = 1;
          } else {
            stop("adj: square matrix expected");
          }
          n9 = p1.tensor.dim[0];
          p2 = alloc_tensor(n9 * n9);
          p2.tensor.ndim = 2;
          p2.tensor.dim[0] = n9;
          p2.tensor.dim[1] = n9;
          for (i5 = o12 = 0, ref2 = n9; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            for (j2 = i12 = 0, ref12 = n9; 0 <= ref12 ? i12 < ref12 : i12 > ref12; j2 = 0 <= ref12 ? ++i12 : --i12) {
              cofactor(p1, n9, i5, j2);
              p2.tensor.elem[n9 * j2 + i5] = pop();
            }
          }
          push(p2);
          return restore();
        };
        Eval_approxratio = function() {
          var theArgument2;
          theArgument2 = cadr(p1);
          push(theArgument2);
          return approxratioRecursive();
        };
        approxratioRecursive = function() {
          var i5, i12, o12, ref2, ref12;
          i5 = 0;
          save();
          p1 = pop();
          if (istensor(p1)) {
            p4 = alloc_tensor(p1.tensor.nelem);
            p4.tensor.ndim = p1.tensor.ndim;
            for (i5 = o12 = 0, ref2 = p1.tensor.ndim; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
              p4.tensor.dim[i5] = p1.tensor.dim[i5];
            }
            for (i5 = i12 = 0, ref12 = p1.tensor.nelem; 0 <= ref12 ? i12 < ref12 : i12 > ref12; i5 = 0 <= ref12 ? ++i12 : --i12) {
              push(p1.tensor.elem[i5]);
              approxratioRecursive();
              p4.tensor.elem[i5] = pop();
              check_tensor_dimensions(p4);
            }
            push(p4);
          } else if (p1.k === DOUBLE) {
            push(p1);
            approxOneRatioOnly();
          } else if (iscons(p1)) {
            push(car(p1));
            approxratioRecursive();
            push(cdr(p1));
            approxratioRecursive();
            cons();
          } else {
            push(p1);
          }
          return restore();
        };
        approxOneRatioOnly = function() {
          var numberOfDigitsAfterTheDot, precision, splitBeforeAndAfterDot, supposedlyTheFloat, theFloat, theRatio;
          zzfloat();
          supposedlyTheFloat = pop();
          if (supposedlyTheFloat.k === DOUBLE) {
            theFloat = supposedlyTheFloat.d;
            splitBeforeAndAfterDot = theFloat.toString().split(".");
            if (splitBeforeAndAfterDot.length === 2) {
              numberOfDigitsAfterTheDot = splitBeforeAndAfterDot[1].length;
              precision = 1 / Math.pow(10, numberOfDigitsAfterTheDot);
              theRatio = floatToRatioRoutine(theFloat, precision);
              push_rational(theRatio[0], theRatio[1]);
            } else {
              push_integer(theFloat);
            }
            return;
          }
          push_symbol(APPROXRATIO);
          push(theArgument);
          return list(2);
        };
        floatToRatioRoutine = function(decimal, AccuracyFactor) {
          var DecimalSign, FractionDenominator, FractionNumerator, PreviousDenominator, ScratchValue, Z2, ret;
          FractionNumerator = void 0;
          FractionDenominator = void 0;
          DecimalSign = void 0;
          Z2 = void 0;
          PreviousDenominator = void 0;
          ScratchValue = void 0;
          ret = [0, 0];
          if (isNaN(decimal)) {
            return ret;
          }
          if (decimal === Infinity) {
            ret[0] = 1;
            ret[1] = 0;
            return ret;
          }
          if (decimal === -Infinity) {
            ret[0] = -1;
            ret[1] = 0;
            return ret;
          }
          if (decimal < 0) {
            DecimalSign = -1;
          } else {
            DecimalSign = 1;
          }
          decimal = Math.abs(decimal);
          if (Math.abs(decimal - Math.floor(decimal)) < AccuracyFactor) {
            FractionNumerator = decimal * DecimalSign;
            FractionDenominator = 1;
            ret[0] = FractionNumerator;
            ret[1] = FractionDenominator;
            return ret;
          }
          if (decimal < 1e-19) {
            FractionNumerator = DecimalSign;
            FractionDenominator = 1e19;
            ret[0] = FractionNumerator;
            ret[1] = FractionDenominator;
            return ret;
          }
          if (decimal > 1e19) {
            FractionNumerator = 1e19 * DecimalSign;
            FractionDenominator = 1;
            ret[0] = FractionNumerator;
            ret[1] = FractionDenominator;
            return ret;
          }
          Z2 = decimal;
          PreviousDenominator = 0;
          FractionDenominator = 1;
          while (true) {
            Z2 = 1 / (Z2 - Math.floor(Z2));
            ScratchValue = FractionDenominator;
            FractionDenominator = FractionDenominator * Math.floor(Z2) + PreviousDenominator;
            PreviousDenominator = ScratchValue;
            FractionNumerator = Math.floor(decimal * FractionDenominator + 0.5);
            if (!(Math.abs(decimal - FractionNumerator / FractionDenominator) > AccuracyFactor && Z2 !== Math.floor(Z2))) {
              break;
            }
          }
          FractionNumerator = DecimalSign * FractionNumerator;
          ret[0] = FractionNumerator;
          ret[1] = FractionDenominator;
          return ret;
        };
        approx_just_an_integer = 0;
        approx_sine_of_rational = 1;
        approx_sine_of_pi_times_rational = 2;
        approx_rationalOfPi = 3;
        approx_radicalOfRatio = 4;
        approx_nothingUseful = 5;
        approx_ratioOfRadical = 6;
        approx_rationalOfE = 7;
        approx_logarithmsOfRationals = 8;
        approx_rationalsOfLogarithms = 9;
        approxRationalsOfRadicals = function(theFloat) {
          var bestResultSoFar, complexity, error, hypothesis, i5, i12, j2, len, likelyMultiplier, minimumComplexity, numberOfDigitsAfterTheDot, o12, precision, ratio, ref2, result, splitBeforeAndAfterDot;
          splitBeforeAndAfterDot = theFloat.toString().split(".");
          if (splitBeforeAndAfterDot.length === 2) {
            numberOfDigitsAfterTheDot = splitBeforeAndAfterDot[1].length;
            precision = 1 / Math.pow(10, numberOfDigitsAfterTheDot);
          } else {
            return ["" + Math.floor(theFloat), approx_just_an_integer, Math.floor(theFloat), 1, 2];
          }
          console.log("precision: " + precision);
          bestResultSoFar = null;
          minimumComplexity = Number.MAX_VALUE;
          ref2 = [2, 3, 5, 6, 7, 8, 10];
          for (o12 = 0, len = ref2.length; o12 < len; o12++) {
            i5 = ref2[o12];
            for (j2 = i12 = 1; i12 <= 10; j2 = ++i12) {
              hypothesis = Math.sqrt(i5) / j2;
              if (Math.abs(hypothesis) > 1e-10) {
                ratio = theFloat / hypothesis;
                likelyMultiplier = Math.round(ratio);
                error = Math.abs(1 - ratio / likelyMultiplier);
              } else {
                ratio = 1;
                likelyMultiplier = 1;
                error = Math.abs(theFloat - hypothesis);
              }
              if (error < 2 * precision) {
                complexity = simpleComplexityMeasure(likelyMultiplier, i5, j2);
                if (complexity < minimumComplexity) {
                  minimumComplexity = complexity;
                  result = likelyMultiplier + " * sqrt( " + i5 + " ) / " + j2;
                  bestResultSoFar = [result, approx_ratioOfRadical, likelyMultiplier, i5, j2];
                }
              }
            }
          }
          return bestResultSoFar;
        };
        approxRadicalsOfRationals = function(theFloat) {
          var bestResultSoFar, complexity, error, hypothesis, i5, i12, j2, len, len1, likelyMultiplier, minimumComplexity, numberOfDigitsAfterTheDot, o12, precision, ratio, ref2, ref12, result, splitBeforeAndAfterDot;
          splitBeforeAndAfterDot = theFloat.toString().split(".");
          if (splitBeforeAndAfterDot.length === 2) {
            numberOfDigitsAfterTheDot = splitBeforeAndAfterDot[1].length;
            precision = 1 / Math.pow(10, numberOfDigitsAfterTheDot);
          } else {
            return ["" + Math.floor(theFloat), approx_just_an_integer, Math.floor(theFloat), 1, 2];
          }
          console.log("precision: " + precision);
          bestResultSoFar = null;
          minimumComplexity = Number.MAX_VALUE;
          ref2 = [1, 2, 3, 5, 6, 7, 8, 10];
          for (o12 = 0, len = ref2.length; o12 < len; o12++) {
            i5 = ref2[o12];
            ref12 = [1, 2, 3, 5, 6, 7, 8, 10];
            for (i12 = 0, len1 = ref12.length; i12 < len1; i12++) {
              j2 = ref12[i12];
              hypothesis = Math.sqrt(i5 / j2);
              if (Math.abs(hypothesis) > 1e-10) {
                ratio = theFloat / hypothesis;
                likelyMultiplier = Math.round(ratio);
                error = Math.abs(1 - ratio / likelyMultiplier);
              } else {
                ratio = 1;
                likelyMultiplier = 1;
                error = Math.abs(theFloat - hypothesis);
              }
              if (error < 2 * precision) {
                complexity = simpleComplexityMeasure(likelyMultiplier, i5, j2);
                if (complexity < minimumComplexity) {
                  minimumComplexity = complexity;
                  result = likelyMultiplier + " * (sqrt( " + i5 + " / " + j2 + " )";
                  bestResultSoFar = [result, approx_radicalOfRatio, likelyMultiplier, i5, j2];
                }
              }
            }
          }
          return bestResultSoFar;
        };
        approxRadicals = function(theFloat) {
          var approxRadicalsOfRationalsResult, approxRationalsOfRadicalsResult, numberOfDigitsAfterTheDot, precision, splitBeforeAndAfterDot;
          splitBeforeAndAfterDot = theFloat.toString().split(".");
          if (splitBeforeAndAfterDot.length === 2) {
            numberOfDigitsAfterTheDot = splitBeforeAndAfterDot[1].length;
            precision = 1 / Math.pow(10, numberOfDigitsAfterTheDot);
          } else {
            return ["" + Math.floor(theFloat), approx_just_an_integer, Math.floor(theFloat), 1, 2];
          }
          console.log("precision: " + precision);
          approxRationalsOfRadicalsResult = approxRationalsOfRadicals(theFloat);
          if (approxRationalsOfRadicalsResult != null) {
            return approxRationalsOfRadicalsResult;
          }
          approxRadicalsOfRationalsResult = approxRadicalsOfRationals(theFloat);
          if (approxRadicalsOfRationalsResult != null) {
            return approxRadicalsOfRationalsResult;
          }
          return null;
        };
        approxLogs = function(theFloat) {
          var approxLogsOfRationalsResult, approxRationalsOfLogsResult, numberOfDigitsAfterTheDot, precision, splitBeforeAndAfterDot;
          splitBeforeAndAfterDot = theFloat.toString().split(".");
          if (splitBeforeAndAfterDot.length === 2) {
            numberOfDigitsAfterTheDot = splitBeforeAndAfterDot[1].length;
            precision = 1 / Math.pow(10, numberOfDigitsAfterTheDot);
          } else {
            return ["" + Math.floor(theFloat), approx_just_an_integer, Math.floor(theFloat), 1, 2];
          }
          console.log("precision: " + precision);
          approxRationalsOfLogsResult = approxRationalsOfLogs(theFloat);
          if (approxRationalsOfLogsResult != null) {
            return approxRationalsOfLogsResult;
          }
          approxLogsOfRationalsResult = approxLogsOfRationals(theFloat);
          if (approxLogsOfRationalsResult != null) {
            return approxLogsOfRationalsResult;
          }
          return null;
        };
        approxRationalsOfLogs = function(theFloat) {
          var bestResultSoFar, complexity, error, hypothesis, i5, i12, j2, likelyMultiplier, minimumComplexity, numberOfDigitsAfterTheDot, o12, precision, ratio, result, splitBeforeAndAfterDot;
          splitBeforeAndAfterDot = theFloat.toString().split(".");
          if (splitBeforeAndAfterDot.length === 2) {
            numberOfDigitsAfterTheDot = splitBeforeAndAfterDot[1].length;
            precision = 1 / Math.pow(10, numberOfDigitsAfterTheDot);
          } else {
            return ["" + Math.floor(theFloat), approx_just_an_integer, Math.floor(theFloat), 1, 2];
          }
          console.log("precision: " + precision);
          bestResultSoFar = null;
          minimumComplexity = Number.MAX_VALUE;
          for (i5 = o12 = 2; o12 <= 5; i5 = ++o12) {
            for (j2 = i12 = 1; i12 <= 5; j2 = ++i12) {
              hypothesis = Math.log(i5) / j2;
              if (Math.abs(hypothesis) > 1e-10) {
                ratio = theFloat / hypothesis;
                likelyMultiplier = Math.round(ratio);
                error = Math.abs(1 - ratio / likelyMultiplier);
              } else {
                ratio = 1;
                likelyMultiplier = 1;
                error = Math.abs(theFloat - hypothesis);
              }
              if (likelyMultiplier !== 1 && Math.abs(Math.floor(likelyMultiplier / j2)) === Math.abs(likelyMultiplier / j2)) {
                continue;
              }
              if (error < 2.2 * precision) {
                complexity = simpleComplexityMeasure(likelyMultiplier, i5, j2);
                if (complexity < minimumComplexity) {
                  minimumComplexity = complexity;
                  result = likelyMultiplier + " * log( " + i5 + " ) / " + j2;
                  bestResultSoFar = [result, approx_rationalsOfLogarithms, likelyMultiplier, i5, j2];
                }
              }
            }
          }
          return bestResultSoFar;
        };
        approxLogsOfRationals = function(theFloat) {
          var bestResultSoFar, complexity, error, hypothesis, i5, i12, j2, likelyMultiplier, minimumComplexity, numberOfDigitsAfterTheDot, o12, precision, ratio, result, splitBeforeAndAfterDot;
          splitBeforeAndAfterDot = theFloat.toString().split(".");
          if (splitBeforeAndAfterDot.length === 2) {
            numberOfDigitsAfterTheDot = splitBeforeAndAfterDot[1].length;
            precision = 1 / Math.pow(10, numberOfDigitsAfterTheDot);
          } else {
            return ["" + Math.floor(theFloat), approx_just_an_integer, Math.floor(theFloat), 1, 2];
          }
          console.log("precision: " + precision);
          bestResultSoFar = null;
          minimumComplexity = Number.MAX_VALUE;
          for (i5 = o12 = 1; o12 <= 5; i5 = ++o12) {
            for (j2 = i12 = 1; i12 <= 5; j2 = ++i12) {
              hypothesis = Math.log(i5 / j2);
              if (Math.abs(hypothesis) > 1e-10) {
                ratio = theFloat / hypothesis;
                likelyMultiplier = Math.round(ratio);
                error = Math.abs(1 - ratio / likelyMultiplier);
              } else {
                ratio = 1;
                likelyMultiplier = 1;
                error = Math.abs(theFloat - hypothesis);
              }
              if (error < 1.96 * precision) {
                complexity = simpleComplexityMeasure(likelyMultiplier, i5, j2);
                if (complexity < minimumComplexity) {
                  minimumComplexity = complexity;
                  result = likelyMultiplier + " * log( " + i5 + " / " + j2 + " )";
                  bestResultSoFar = [result, approx_logarithmsOfRationals, likelyMultiplier, i5, j2];
                }
              }
            }
          }
          return bestResultSoFar;
        };
        approxRationalsOfPowersOfE = function(theFloat) {
          var bestResultSoFar, complexity, error, hypothesis, i5, i12, j2, likelyMultiplier, minimumComplexity, numberOfDigitsAfterTheDot, o12, precision, ratio, result, splitBeforeAndAfterDot;
          splitBeforeAndAfterDot = theFloat.toString().split(".");
          if (splitBeforeAndAfterDot.length === 2) {
            numberOfDigitsAfterTheDot = splitBeforeAndAfterDot[1].length;
            precision = 1 / Math.pow(10, numberOfDigitsAfterTheDot);
          } else {
            return ["" + Math.floor(theFloat), approx_just_an_integer, Math.floor(theFloat), 1, 2];
          }
          console.log("precision: " + precision);
          bestResultSoFar = null;
          minimumComplexity = Number.MAX_VALUE;
          for (i5 = o12 = 1; o12 <= 2; i5 = ++o12) {
            for (j2 = i12 = 1; i12 <= 12; j2 = ++i12) {
              hypothesis = Math.pow(Math.E, i5) / j2;
              if (Math.abs(hypothesis) > 1e-10) {
                ratio = theFloat / hypothesis;
                likelyMultiplier = Math.round(ratio);
                error = Math.abs(1 - ratio / likelyMultiplier);
              } else {
                ratio = 1;
                likelyMultiplier = 1;
                error = Math.abs(theFloat - hypothesis);
              }
              if (error < 2 * precision) {
                complexity = simpleComplexityMeasure(likelyMultiplier, i5, j2);
                if (complexity < minimumComplexity) {
                  minimumComplexity = complexity;
                  result = likelyMultiplier + " * (e ^ " + i5 + " ) / " + j2;
                  bestResultSoFar = [result, approx_rationalOfE, likelyMultiplier, i5, j2];
                }
              }
            }
          }
          return bestResultSoFar;
        };
        approxRationalsOfPowersOfPI = function(theFloat) {
          var bestResultSoFar, complexity, error, hypothesis, i5, i12, j2, likelyMultiplier, minimumComplexity, numberOfDigitsAfterTheDot, o12, precision, ratio, result, splitBeforeAndAfterDot;
          splitBeforeAndAfterDot = theFloat.toString().split(".");
          if (splitBeforeAndAfterDot.length === 2) {
            numberOfDigitsAfterTheDot = splitBeforeAndAfterDot[1].length;
            precision = 1 / Math.pow(10, numberOfDigitsAfterTheDot);
          } else {
            return ["" + Math.floor(theFloat), approx_just_an_integer, Math.floor(theFloat), 1, 2];
          }
          console.log("precision: " + precision);
          bestResultSoFar = null;
          minimumComplexity = Number.MAX_VALUE;
          for (i5 = o12 = 1; o12 <= 5; i5 = ++o12) {
            for (j2 = i12 = 1; i12 <= 12; j2 = ++i12) {
              hypothesis = Math.pow(Math.PI, i5) / j2;
              if (Math.abs(hypothesis) > 1e-10) {
                ratio = theFloat / hypothesis;
                likelyMultiplier = Math.round(ratio);
                error = Math.abs(1 - ratio / likelyMultiplier);
              } else {
                ratio = 1;
                likelyMultiplier = 1;
                error = Math.abs(theFloat - hypothesis);
              }
              if (error < 2 * precision) {
                complexity = simpleComplexityMeasure(likelyMultiplier, i5, j2);
                if (complexity < minimumComplexity) {
                  minimumComplexity = complexity;
                  result = likelyMultiplier + " * (pi ^ " + i5 + " ) / " + j2 + " )";
                  bestResultSoFar = [result, approx_rationalOfPi, likelyMultiplier, i5, j2];
                }
              }
            }
          }
          return bestResultSoFar;
        };
        approxTrigonometric = function(theFloat) {
          var approxSineOfRationalMultiplesOfPIResult, approxSineOfRationalsResult, numberOfDigitsAfterTheDot, precision, splitBeforeAndAfterDot;
          splitBeforeAndAfterDot = theFloat.toString().split(".");
          if (splitBeforeAndAfterDot.length === 2) {
            numberOfDigitsAfterTheDot = splitBeforeAndAfterDot[1].length;
            precision = 1 / Math.pow(10, numberOfDigitsAfterTheDot);
          } else {
            return ["" + Math.floor(theFloat), approx_just_an_integer, Math.floor(theFloat), 1, 2];
          }
          console.log("precision: " + precision);
          approxSineOfRationalsResult = approxSineOfRationals(theFloat);
          if (approxSineOfRationalsResult != null) {
            return approxSineOfRationalsResult;
          }
          approxSineOfRationalMultiplesOfPIResult = approxSineOfRationalMultiplesOfPI(theFloat);
          if (approxSineOfRationalMultiplesOfPIResult != null) {
            return approxSineOfRationalMultiplesOfPIResult;
          }
          return null;
        };
        approxSineOfRationals = function(theFloat) {
          var bestResultSoFar, complexity, error, fraction, hypothesis, i5, i12, j2, likelyMultiplier, minimumComplexity, numberOfDigitsAfterTheDot, o12, precision, ratio, result, splitBeforeAndAfterDot;
          splitBeforeAndAfterDot = theFloat.toString().split(".");
          if (splitBeforeAndAfterDot.length === 2) {
            numberOfDigitsAfterTheDot = splitBeforeAndAfterDot[1].length;
            precision = 1 / Math.pow(10, numberOfDigitsAfterTheDot);
          } else {
            return ["" + Math.floor(theFloat), approx_just_an_integer, Math.floor(theFloat), 1, 2];
          }
          console.log("precision: " + precision);
          bestResultSoFar = null;
          minimumComplexity = Number.MAX_VALUE;
          for (i5 = o12 = 1; o12 <= 4; i5 = ++o12) {
            for (j2 = i12 = 1; i12 <= 4; j2 = ++i12) {
              fraction = i5 / j2;
              hypothesis = Math.sin(fraction);
              if (Math.abs(hypothesis) > 1e-10) {
                ratio = theFloat / hypothesis;
                likelyMultiplier = Math.round(ratio);
                error = Math.abs(1 - ratio / likelyMultiplier);
              } else {
                ratio = 1;
                likelyMultiplier = 1;
                error = Math.abs(theFloat - hypothesis);
              }
              if (error < 2 * precision) {
                complexity = simpleComplexityMeasure(likelyMultiplier, i5, j2);
                if (complexity < minimumComplexity) {
                  minimumComplexity = complexity;
                  result = likelyMultiplier + " * sin( " + i5 + "/" + j2 + " )";
                  bestResultSoFar = [result, approx_sine_of_rational, likelyMultiplier, i5, j2];
                }
              }
            }
          }
          return bestResultSoFar;
        };
        approxSineOfRationalMultiplesOfPI = function(theFloat) {
          var bestResultSoFar, complexity, error, fraction, hypothesis, i5, i12, j2, likelyMultiplier, minimumComplexity, numberOfDigitsAfterTheDot, o12, precision, ratio, result, splitBeforeAndAfterDot;
          splitBeforeAndAfterDot = theFloat.toString().split(".");
          if (splitBeforeAndAfterDot.length === 2) {
            numberOfDigitsAfterTheDot = splitBeforeAndAfterDot[1].length;
            precision = 1 / Math.pow(10, numberOfDigitsAfterTheDot);
          } else {
            return ["" + Math.floor(theFloat), approx_just_an_integer, Math.floor(theFloat), 1, 2];
          }
          console.log("precision: " + precision);
          bestResultSoFar = null;
          minimumComplexity = Number.MAX_VALUE;
          for (i5 = o12 = 1; o12 <= 13; i5 = ++o12) {
            for (j2 = i12 = 1; i12 <= 13; j2 = ++i12) {
              fraction = i5 / j2;
              hypothesis = Math.sin(Math.PI * fraction);
              if (Math.abs(hypothesis) > 1e-10) {
                ratio = theFloat / hypothesis;
                likelyMultiplier = Math.round(ratio);
                error = Math.abs(1 - ratio / likelyMultiplier);
              } else {
                ratio = 1;
                likelyMultiplier = 1;
                error = Math.abs(theFloat - hypothesis);
              }
              if (error < 23 * precision) {
                complexity = simpleComplexityMeasure(likelyMultiplier, i5, j2);
                if (complexity < minimumComplexity) {
                  minimumComplexity = complexity;
                  result = likelyMultiplier + " * sin( " + i5 + "/" + j2 + " * pi )";
                  bestResultSoFar = [result, approx_sine_of_pi_times_rational, likelyMultiplier, i5, j2];
                }
              }
            }
          }
          return bestResultSoFar;
        };
        approxAll = function(theFloat) {
          var LOG_EXPLANATIONS, approxLogsResult, approxRadicalsResult, approxRationalsOfPowersOfEResult, approxRationalsOfPowersOfPIResult, approxTrigonometricResult, bestApproxSoFar, constantsSum, constantsSumMin, numberOfDigitsAfterTheDot, precision, splitBeforeAndAfterDot;
          splitBeforeAndAfterDot = theFloat.toString().split(".");
          if (splitBeforeAndAfterDot.length === 2) {
            numberOfDigitsAfterTheDot = splitBeforeAndAfterDot[1].length;
            precision = 1 / Math.pow(10, numberOfDigitsAfterTheDot);
          } else {
            return ["" + Math.floor(theFloat), approx_just_an_integer, Math.floor(theFloat), 1, 2];
          }
          console.log("precision: " + precision);
          constantsSumMin = Number.MAX_VALUE;
          constantsSum = 0;
          bestApproxSoFar = null;
          LOG_EXPLANATIONS = true;
          approxRadicalsResult = approxRadicals(theFloat);
          if (approxRadicalsResult != null) {
            constantsSum = simpleComplexityMeasure(approxRadicalsResult);
            if (constantsSum < constantsSumMin) {
              if (LOG_EXPLANATIONS) {
                console.log("better explanation by approxRadicals: " + approxRadicalsResult + " complexity: " + constantsSum);
              }
              constantsSumMin = constantsSum;
              bestApproxSoFar = approxRadicalsResult;
            } else {
              if (LOG_EXPLANATIONS) {
                console.log("subpar explanation by approxRadicals: " + approxRadicalsResult + " complexity: " + constantsSum);
              }
            }
          }
          approxLogsResult = approxLogs(theFloat);
          if (approxLogsResult != null) {
            constantsSum = simpleComplexityMeasure(approxLogsResult);
            if (constantsSum < constantsSumMin) {
              if (LOG_EXPLANATIONS) {
                console.log("better explanation by approxLogs: " + approxLogsResult + " complexity: " + constantsSum);
              }
              constantsSumMin = constantsSum;
              bestApproxSoFar = approxLogsResult;
            } else {
              if (LOG_EXPLANATIONS) {
                console.log("subpar explanation by approxLogs: " + approxLogsResult + " complexity: " + constantsSum);
              }
            }
          }
          approxRationalsOfPowersOfEResult = approxRationalsOfPowersOfE(theFloat);
          if (approxRationalsOfPowersOfEResult != null) {
            constantsSum = simpleComplexityMeasure(approxRationalsOfPowersOfEResult);
            if (constantsSum < constantsSumMin) {
              if (LOG_EXPLANATIONS) {
                console.log("better explanation by approxRationalsOfPowersOfE: " + approxRationalsOfPowersOfEResult + " complexity: " + constantsSum);
              }
              constantsSumMin = constantsSum;
              bestApproxSoFar = approxRationalsOfPowersOfEResult;
            } else {
              if (LOG_EXPLANATIONS) {
                console.log("subpar explanation by approxRationalsOfPowersOfE: " + approxRationalsOfPowersOfEResult + " complexity: " + constantsSum);
              }
            }
          }
          approxRationalsOfPowersOfPIResult = approxRationalsOfPowersOfPI(theFloat);
          if (approxRationalsOfPowersOfPIResult != null) {
            constantsSum = simpleComplexityMeasure(approxRationalsOfPowersOfPIResult);
            if (constantsSum < constantsSumMin) {
              if (LOG_EXPLANATIONS) {
                console.log("better explanation by approxRationalsOfPowersOfPI: " + approxRationalsOfPowersOfPIResult + " complexity: " + constantsSum);
              }
              constantsSumMin = constantsSum;
              bestApproxSoFar = approxRationalsOfPowersOfPIResult;
            } else {
              if (LOG_EXPLANATIONS) {
                console.log("subpar explanation by approxRationalsOfPowersOfPI: " + approxRationalsOfPowersOfPIResult + " complexity: " + constantsSum);
              }
            }
          }
          approxTrigonometricResult = approxTrigonometric(theFloat);
          if (approxTrigonometricResult != null) {
            constantsSum = simpleComplexityMeasure(approxTrigonometricResult);
            if (constantsSum < constantsSumMin) {
              if (LOG_EXPLANATIONS) {
                console.log("better explanation by approxTrigonometric: " + approxTrigonometricResult + " complexity: " + constantsSum);
              }
              constantsSumMin = constantsSum;
              bestApproxSoFar = approxTrigonometricResult;
            } else {
              if (LOG_EXPLANATIONS) {
                console.log("subpar explanation by approxTrigonometric: " + approxTrigonometricResult + " complexity: " + constantsSum);
              }
            }
          }
          return bestApproxSoFar;
        };
        simpleComplexityMeasure = function(aResult, b2, c6) {
          var theSum;
          theSum = null;
          if (aResult instanceof Array) {
            switch (aResult[1]) {
              case approx_sine_of_pi_times_rational:
                theSum = 4;
                break;
              // exponents of PI and E need to be penalised as well
              // otherwise they come to explain any big number
              // so we count them just as much as the multiplier
              case approx_rationalOfPi:
                theSum = Math.pow(4, Math.abs(aResult[3])) * Math.abs(aResult[2]);
                break;
              case approx_rationalOfE:
                theSum = Math.pow(3, Math.abs(aResult[3])) * Math.abs(aResult[2]);
                break;
              default:
                theSum = 0;
            }
            theSum += Math.abs(aResult[2]) * (Math.abs(aResult[3]) + Math.abs(aResult[4]));
          } else {
            theSum += Math.abs(aResult) * (Math.abs(b2) + Math.abs(c6));
          }
          if (aResult[2] === 1) {
            theSum -= 1;
          } else {
            theSum += 1;
          }
          if (aResult[3] === 1) {
            theSum -= 1;
          } else {
            theSum += 1;
          }
          if (aResult[4] === 1) {
            theSum -= 1;
          } else {
            theSum += 1;
          }
          if (theSum < 0) {
            theSum = 0;
          }
          return theSum;
        };
        testApprox = function() {
          var error, fraction, i5, i12, i22, i32, j2, j12, j22, j3, k3, l1, l22, l32, len, len1, len2, len3, len4, len5, len6, len7, m1, m22, m3, n1, n22, o12, o1, o22, originalValue, q1, q2, r1, r22, ref2, ref12, ref22, ref3, ref4, ref5, ref6, ref7, returned, returnedFraction, returnedValue, s1, s22, t1, t22, u1, u22, v1, v2, value, x1, x2, z1, z2;
          ref2 = [2, 3, 5, 6, 7, 8, 10];
          for (o12 = 0, len = ref2.length; o12 < len; o12++) {
            i5 = ref2[o12];
            ref12 = [2, 3, 5, 6, 7, 8, 10];
            for (i12 = 0, len1 = ref12.length; i12 < len1; i12++) {
              j2 = ref12[i12];
              if (i5 === j2) {
                continue;
              }
              console.log("testapproxRadicals testing: 1 * sqrt( " + i5 + " ) / " + j2);
              fraction = i5 / j2;
              value = Math.sqrt(i5) / j2;
              returned = approxRadicals(value);
              returnedValue = returned[2] * Math.sqrt(returned[3]) / returned[4];
              if (Math.abs(value - returnedValue) > 1e-15) {
                console.log("fail testapproxRadicals: 1 * sqrt( " + i5 + " ) / " + j2 + " . obtained: " + returned);
              }
            }
          }
          ref22 = [2, 3, 5, 6, 7, 8, 10];
          for (j12 = 0, len2 = ref22.length; j12 < len2; j12++) {
            i5 = ref22[j12];
            ref3 = [2, 3, 5, 6, 7, 8, 10];
            for (l1 = 0, len3 = ref3.length; l1 < len3; l1++) {
              j2 = ref3[l1];
              if (i5 === j2) {
                continue;
              }
              console.log("testapproxRadicals testing with 4 digits: 1 * sqrt( " + i5 + " ) / " + j2);
              fraction = i5 / j2;
              originalValue = Math.sqrt(i5) / j2;
              value = originalValue.toFixed(4);
              returned = approxRadicals(value);
              returnedValue = returned[2] * Math.sqrt(returned[3]) / returned[4];
              if (Math.abs(originalValue - returnedValue) > 1e-15) {
                console.log("fail testapproxRadicals with 4 digits: 1 * sqrt( " + i5 + " ) / " + j2 + " . obtained: " + returned);
              }
            }
          }
          ref4 = [2, 3, 5, 6, 7, 8, 10];
          for (m1 = 0, len4 = ref4.length; m1 < len4; m1++) {
            i5 = ref4[m1];
            ref5 = [2, 3, 5, 6, 7, 8, 10];
            for (n1 = 0, len5 = ref5.length; n1 < len5; n1++) {
              j2 = ref5[n1];
              if (i5 === j2) {
                continue;
              }
              console.log("testapproxRadicals testing: 1 * sqrt( " + i5 + " / " + j2 + " )");
              fraction = i5 / j2;
              value = Math.sqrt(i5 / j2);
              returned = approxRadicals(value);
              if (returned != null) {
                returnedValue = returned[2] * Math.sqrt(returned[3] / returned[4]);
                if (returned[1] === approx_radicalOfRatio && Math.abs(value - returnedValue) > 1e-15) {
                  console.log("fail testapproxRadicals: 1 * sqrt( " + i5 + " / " + j2 + " ) . obtained: " + returned);
                }
              }
            }
          }
          ref6 = [1, 2, 3, 5, 6, 7, 8, 10];
          for (o1 = 0, len6 = ref6.length; o1 < len6; o1++) {
            i5 = ref6[o1];
            ref7 = [1, 2, 3, 5, 6, 7, 8, 10];
            for (q1 = 0, len7 = ref7.length; q1 < len7; q1++) {
              j2 = ref7[q1];
              if (i5 === 1 && j2 === 1) {
                continue;
              }
              console.log("testapproxRadicals testing with 4 digits:: 1 * sqrt( " + i5 + " / " + j2 + " )");
              fraction = i5 / j2;
              originalValue = Math.sqrt(i5 / j2);
              value = originalValue.toFixed(4);
              returned = approxRadicals(value);
              returnedValue = returned[2] * Math.sqrt(returned[3] / returned[4]);
              if (returned[1] === approx_radicalOfRatio && Math.abs(originalValue - returnedValue) > 1e-15) {
                console.log("fail testapproxRadicals with 4 digits:: 1 * sqrt( " + i5 + " / " + j2 + " ) . obtained: " + returned);
              }
            }
          }
          for (i5 = r1 = 1; r1 <= 5; i5 = ++r1) {
            for (j2 = s1 = 1; s1 <= 5; j2 = ++s1) {
              console.log("testApproxAll testing: 1 * log(" + i5 + " ) / " + j2);
              fraction = i5 / j2;
              value = Math.log(i5) / j2;
              returned = approxAll(value);
              returnedValue = returned[2] * Math.log(returned[3]) / returned[4];
              if (Math.abs(value - returnedValue) > 1e-15) {
                console.log("fail testApproxAll: 1 * log(" + i5 + " ) / " + j2 + " . obtained: " + returned);
              }
            }
          }
          for (i5 = t1 = 1; t1 <= 5; i5 = ++t1) {
            for (j2 = u1 = 1; u1 <= 5; j2 = ++u1) {
              console.log("testApproxAll testing with 4 digits: 1 * log(" + i5 + " ) / " + j2);
              fraction = i5 / j2;
              originalValue = Math.log(i5) / j2;
              value = originalValue.toFixed(4);
              returned = approxAll(value);
              returnedValue = returned[2] * Math.log(returned[3]) / returned[4];
              if (Math.abs(originalValue - returnedValue) > 1e-15) {
                console.log("fail testApproxAll with 4 digits: 1 * log(" + i5 + " ) / " + j2 + " . obtained: " + returned);
              }
            }
          }
          for (i5 = v1 = 1; v1 <= 5; i5 = ++v1) {
            for (j2 = x1 = 1; x1 <= 5; j2 = ++x1) {
              console.log("testApproxAll testing: 1 * log(" + i5 + " / " + j2 + " )");
              fraction = i5 / j2;
              value = Math.log(i5 / j2);
              returned = approxAll(value);
              returnedValue = returned[2] * Math.log(returned[3] / returned[4]);
              if (Math.abs(value - returnedValue) > 1e-15) {
                console.log("fail testApproxAll: 1 * log(" + i5 + " / " + j2 + " ) . obtained: " + returned);
              }
            }
          }
          for (i5 = z1 = 1; z1 <= 5; i5 = ++z1) {
            for (j2 = i22 = 1; i22 <= 5; j2 = ++i22) {
              console.log("testApproxAll testing with 4 digits: 1 * log(" + i5 + " / " + j2 + " )");
              fraction = i5 / j2;
              originalValue = Math.log(i5 / j2);
              value = originalValue.toFixed(4);
              returned = approxAll(value);
              returnedValue = returned[2] * Math.log(returned[3] / returned[4]);
              if (Math.abs(originalValue - returnedValue) > 1e-15) {
                console.log("fail testApproxAll with 4 digits: 1 * log(" + i5 + " / " + j2 + " ) . obtained: " + returned);
              }
            }
          }
          for (i5 = j22 = 1; j22 <= 2; i5 = ++j22) {
            for (j2 = l22 = 1; l22 <= 12; j2 = ++l22) {
              console.log("testApproxAll testing: 1 * (e ^ " + i5 + " ) / " + j2);
              fraction = i5 / j2;
              value = Math.pow(Math.E, i5) / j2;
              returned = approxAll(value);
              returnedValue = returned[2] * Math.pow(Math.E, returned[3]) / returned[4];
              if (Math.abs(value - returnedValue) > 1e-15) {
                console.log("fail testApproxAll: 1 * (e ^ " + i5 + " ) / " + j2 + " . obtained: " + returned);
              }
            }
          }
          for (i5 = m22 = 1; m22 <= 2; i5 = ++m22) {
            for (j2 = n22 = 1; n22 <= 12; j2 = ++n22) {
              console.log("approxRationalsOfPowersOfE testing with 4 digits: 1 * (e ^ " + i5 + " ) / " + j2);
              fraction = i5 / j2;
              originalValue = Math.pow(Math.E, i5) / j2;
              value = originalValue.toFixed(4);
              returned = approxRationalsOfPowersOfE(value);
              returnedValue = returned[2] * Math.pow(Math.E, returned[3]) / returned[4];
              if (Math.abs(originalValue - returnedValue) > 1e-15) {
                console.log("fail approxRationalsOfPowersOfE with 4 digits: 1 * (e ^ " + i5 + " ) / " + j2 + " . obtained: " + returned);
              }
            }
          }
          for (i5 = o22 = 1; o22 <= 2; i5 = ++o22) {
            for (j2 = q2 = 1; q2 <= 12; j2 = ++q2) {
              console.log("testApproxAll testing: 1 * pi ^ " + i5 + " / " + j2);
              fraction = i5 / j2;
              value = Math.pow(Math.PI, i5) / j2;
              returned = approxAll(value);
              returnedValue = returned[2] * Math.pow(Math.PI, returned[3]) / returned[4];
              if (Math.abs(value - returnedValue) > 1e-15) {
                console.log("fail testApproxAll: 1 * pi ^ " + i5 + " / " + j2 + " ) . obtained: " + returned);
              }
            }
          }
          for (i5 = r22 = 1; r22 <= 2; i5 = ++r22) {
            for (j2 = s22 = 1; s22 <= 12; j2 = ++s22) {
              console.log("approxRationalsOfPowersOfPI testing with 4 digits: 1 * pi ^ " + i5 + " / " + j2);
              fraction = i5 / j2;
              originalValue = Math.pow(Math.PI, i5) / j2;
              value = originalValue.toFixed(4);
              returned = approxRationalsOfPowersOfPI(value);
              returnedValue = returned[2] * Math.pow(Math.PI, returned[3]) / returned[4];
              if (Math.abs(originalValue - returnedValue) > 1e-15) {
                console.log("fail approxRationalsOfPowersOfPI with 4 digits: 1 * pi ^ " + i5 + " / " + j2 + " ) . obtained: " + returned);
              }
            }
          }
          for (i5 = t22 = 1; t22 <= 4; i5 = ++t22) {
            for (j2 = u22 = 1; u22 <= 4; j2 = ++u22) {
              console.log("testApproxAll testing: 1 * sin( " + i5 + "/" + j2 + " )");
              fraction = i5 / j2;
              value = Math.sin(fraction);
              returned = approxAll(value);
              returnedFraction = returned[3] / returned[4];
              returnedValue = returned[2] * Math.sin(returnedFraction);
              if (Math.abs(value - returnedValue) > 1e-15) {
                console.log("fail testApproxAll: 1 * sin( " + i5 + "/" + j2 + " ) . obtained: " + returned);
              }
            }
          }
          for (i5 = v2 = 1; v2 <= 4; i5 = ++v2) {
            for (j2 = x2 = 1; x2 <= 4; j2 = ++x2) {
              console.log("testApproxAll testing with 5 digits: 1 * sin( " + i5 + "/" + j2 + " )");
              fraction = i5 / j2;
              originalValue = Math.sin(fraction);
              value = originalValue.toFixed(5);
              returned = approxAll(value);
              if (returned == null) {
                console.log("fail testApproxAll with 5 digits: 1 * sin( " + i5 + "/" + j2 + " ) . obtained:  undefined ");
              }
              returnedFraction = returned[3] / returned[4];
              returnedValue = returned[2] * Math.sin(returnedFraction);
              error = Math.abs(originalValue - returnedValue);
              if (error > 1e-14) {
                console.log("fail testApproxAll with 5 digits: 1 * sin( " + i5 + "/" + j2 + " ) . obtained: " + returned + " error: " + error);
              }
            }
          }
          for (i5 = z2 = 1; z2 <= 4; i5 = ++z2) {
            for (j2 = i32 = 1; i32 <= 4; j2 = ++i32) {
              console.log("testApproxAll testing with 4 digits: 1 * sin( " + i5 + "/" + j2 + " )");
              fraction = i5 / j2;
              originalValue = Math.sin(fraction);
              value = originalValue.toFixed(4);
              returned = approxAll(value);
              if (returned == null) {
                console.log("fail testApproxAll with 4 digits: 1 * sin( " + i5 + "/" + j2 + " ) . obtained:  undefined ");
              }
              returnedFraction = returned[3] / returned[4];
              returnedValue = returned[2] * Math.sin(returnedFraction);
              error = Math.abs(originalValue - returnedValue);
              if (error > 1e-14) {
                console.log("fail testApproxAll with 4 digits: 1 * sin( " + i5 + "/" + j2 + " ) . obtained: " + returned + " error: " + error);
              }
            }
          }
          value = 0;
          if (approxAll(value)[0] !== "0") {
            console.log("fail testApproxAll: 0");
          }
          value = 0;
          if (approxAll(value)[0] !== "0") {
            console.log("fail testApproxAll: 0.0");
          }
          value = 0;
          if (approxAll(value)[0] !== "0") {
            console.log("fail testApproxAll: 0.00");
          }
          value = 0;
          if (approxAll(value)[0] !== "0") {
            console.log("fail testApproxAll: 0.000");
          }
          value = 0;
          if (approxAll(value)[0] !== "0") {
            console.log("fail testApproxAll: 0.0000");
          }
          value = 1;
          if (approxAll(value)[0] !== "1") {
            console.log("fail testApproxAll: 1");
          }
          value = 1;
          if (approxAll(value)[0] !== "1") {
            console.log("fail testApproxAll: 1.0");
          }
          value = 1;
          if (approxAll(value)[0] !== "1") {
            console.log("fail testApproxAll: 1.00");
          }
          value = 1;
          if (approxAll(value)[0] !== "1") {
            console.log("fail testApproxAll: 1.000");
          }
          value = 1;
          if (approxAll(value)[0] !== "1") {
            console.log("fail testApproxAll: 1.0000");
          }
          value = 1;
          if (approxAll(value)[0] !== "1") {
            console.log("fail testApproxAll: 1.00000");
          }
          value = Math.sqrt(2);
          if (approxAll(value)[0] !== "1 * sqrt( 2 ) / 1") {
            console.log("fail testApproxAll: Math.sqrt(2)");
          }
          value = 1.41;
          if (approxAll(value)[0] !== "1 * sqrt( 2 ) / 1") {
            console.log("fail testApproxAll: 1.41");
          }
          value = 1.4;
          if (approxRadicals(value)[0] !== "1 * sqrt( 2 ) / 1") {
            console.log("fail approxRadicals: 1.4");
          }
          value = 0.6;
          if (approxLogs(value)[0] !== "1 * log( 2 ) / 1") {
            console.log("fail approxLogs: 0.6");
          }
          value = 0.69;
          if (approxLogs(value)[0] !== "1 * log( 2 ) / 1") {
            console.log("fail approxLogs: 0.69");
          }
          value = 0.7;
          if (approxLogs(value)[0] !== "1 * log( 2 ) / 1") {
            console.log("fail approxLogs: 0.7");
          }
          value = 1.09;
          if (approxLogs(value)[0] !== "1 * log( 3 ) / 1") {
            console.log("fail approxLogs: 1.09");
          }
          value = 1.09;
          if (approxAll(value)[0] !== "1 * log( 3 ) / 1") {
            console.log("fail approxAll: 1.09");
          }
          value = 1.098;
          if (approxAll(value)[0] !== "1 * log( 3 ) / 1") {
            console.log("fail approxAll: 1.098");
          }
          value = 1.1;
          if (approxAll(value)[0] !== "1 * log( 3 ) / 1") {
            console.log("fail approxAll: 1.1");
          }
          value = 1.11;
          if (approxAll(value)[0] !== "1 * log( 3 ) / 1") {
            console.log("fail approxAll: 1.11");
          }
          value = Math.sqrt(3);
          if (approxAll(value)[0] !== "1 * sqrt( 3 ) / 1") {
            console.log("fail testApproxAll: Math.sqrt(3)");
          }
          value = 1;
          if (approxAll(value)[0] !== "1") {
            console.log("fail testApproxAll: 1.0000");
          }
          value = 3.141592;
          if (approxAll(value)[0] !== "1 * (pi ^ 1 ) / 1 )") {
            console.log("fail testApproxAll: 3.141592");
          }
          value = 31.41592;
          if (approxAll(value)[0] !== "10 * (pi ^ 1 ) / 1 )") {
            console.log("fail testApproxAll: 31.41592");
          }
          value = 314.1592;
          if (approxAll(value)[0] !== "100 * (pi ^ 1 ) / 1 )") {
            console.log("fail testApproxAll: 314.1592");
          }
          value = 3141592653589793e-8;
          if (approxAll(value)[0] !== "10000000 * (pi ^ 1 ) / 1 )") {
            console.log("fail testApproxAll: 31415926.53589793");
          }
          value = Math.sqrt(2);
          if (approxTrigonometric(value)[0] !== "2 * sin( 1/4 * pi )") {
            console.log("fail approxTrigonometric: Math.sqrt(2)");
          }
          value = Math.sqrt(3);
          if (approxTrigonometric(value)[0] !== "2 * sin( 1/3 * pi )") {
            console.log("fail approxTrigonometric: Math.sqrt(3)");
          }
          value = (Math.sqrt(6) - Math.sqrt(2)) / 4;
          if (approxAll(value)[0] !== "1 * sin( 1/12 * pi )") {
            console.log("fail testApproxAll: (Math.sqrt(6) - Math.sqrt(2))/4");
          }
          value = Math.sqrt(2 - Math.sqrt(2)) / 2;
          if (approxAll(value)[0] !== "1 * sin( 1/8 * pi )") {
            console.log("fail testApproxAll: Math.sqrt(2 - Math.sqrt(2))/2");
          }
          value = (Math.sqrt(6) + Math.sqrt(2)) / 4;
          if (approxAll(value)[0] !== "1 * sin( 5/12 * pi )") {
            console.log("fail testApproxAll: (Math.sqrt(6) + Math.sqrt(2))/4");
          }
          value = Math.sqrt(2 + Math.sqrt(3)) / 2;
          if (approxAll(value)[0] !== "1 * sin( 5/12 * pi )") {
            console.log("fail testApproxAll: Math.sqrt(2 + Math.sqrt(3))/2");
          }
          value = (Math.sqrt(5) - 1) / 4;
          if (approxAll(value)[0] !== "1 * sin( 1/10 * pi )") {
            console.log("fail testApproxAll: (Math.sqrt(5) - 1)/4");
          }
          value = Math.sqrt(10 - 2 * Math.sqrt(5)) / 4;
          if (approxAll(value)[0] !== "1 * sin( 1/5 * pi )") {
            console.log("fail testApproxAll: Math.sqrt(10 - 2*Math.sqrt(5))/4");
          }
          value = Math.sin(Math.PI / 7);
          if (approxAll(value)[0] !== "1 * sin( 1/7 * pi )") {
            console.log("fail testApproxAll: Math.sin(Math.PI/7)");
          }
          value = Math.sin(Math.PI / 9);
          if (approxAll(value)[0] !== "1 * sin( 1/9 * pi )") {
            console.log("fail testApproxAll: Math.sin(Math.PI/9)");
          }
          value = 1836.15267;
          if (approxRationalsOfPowersOfPI(value)[0] !== "6 * (pi ^ 5 ) / 1 )") {
            console.log("fail approxRationalsOfPowersOfPI: 1836.15267");
          }
          for (i5 = j3 = 1; j3 <= 13; i5 = ++j3) {
            for (j2 = k3 = 1; k3 <= 13; j2 = ++k3) {
              console.log("approxTrigonometric testing: 1 * sin( " + i5 + "/" + j2 + " * pi )");
              fraction = i5 / j2;
              value = Math.sin(Math.PI * fraction);
              returned = approxTrigonometric(value);
              returnedFraction = returned[3] / returned[4];
              returnedValue = returned[2] * Math.sin(Math.PI * returnedFraction);
              if (Math.abs(value - returnedValue) > 1e-15) {
                console.log("fail approxTrigonometric: 1 * sin( " + i5 + "/" + j2 + " * pi ) . obtained: " + returned);
              }
            }
          }
          for (i5 = l32 = 1; l32 <= 13; i5 = ++l32) {
            for (j2 = m3 = 1; m3 <= 13; j2 = ++m3) {
              if (i5 === 5 && j2 === 11 || i5 === 6 && j2 === 11) {
                continue;
              }
              console.log("approxTrigonometric testing with 4 digits: 1 * sin( " + i5 + "/" + j2 + " * pi )");
              fraction = i5 / j2;
              originalValue = Math.sin(Math.PI * fraction);
              value = originalValue.toFixed(4);
              returned = approxTrigonometric(value);
              returnedFraction = returned[3] / returned[4];
              returnedValue = returned[2] * Math.sin(Math.PI * returnedFraction);
              error = Math.abs(originalValue - returnedValue);
              if (error > 1e-14) {
                console.log("fail approxTrigonometric with 4 digits: 1 * sin( " + i5 + "/" + j2 + " * pi ) . obtained: " + returned + " error: " + error);
              }
            }
          }
          return console.log("testApprox done");
        };
        $.approxRadicals = approxRadicals;
        $.approxRationalsOfLogs = approxRationalsOfLogs;
        $.approxAll = approxAll;
        $.testApprox = testApprox;
        Eval_arccos = function() {
          push(cadr(p1));
          Eval();
          return arccos();
        };
        arccos = function() {
          var d3, errno, n9;
          n9 = 0;
          d3 = 0;
          save();
          p1 = pop();
          if (car(p1) === symbol(COS)) {
            push(cadr(p1));
            restore();
            return;
          }
          if (isdouble(p1)) {
            errno = 0;
            d3 = Math.acos(p1.d);
            if (errno) {
              stop("arccos function argument is not in the interval [-1,1]");
            }
            push_double(d3);
            restore();
            return;
          }
          if (isoneoversqrttwo(p1) || car(p1) === symbol(MULTIPLY) && equalq(car(cdr(p1)), 1, 2) && car(car(cdr(cdr(p1)))) === symbol(POWER) && equaln(car(cdr(car(cdr(cdr(p1))))), 2) && equalq(car(cdr(cdr(car(cdr(cdr(p1)))))), 1, 2)) {
            if (evaluatingAsFloats) {
              push_double(Math.PI / 4);
            } else {
              push_rational(1, 4);
              push_symbol(PI);
              multiply();
            }
            restore();
            return;
          }
          if (isminusoneoversqrttwo(p1) || car(p1) === symbol(MULTIPLY) && equalq(car(cdr(p1)), -1, 2) && car(car(cdr(cdr(p1)))) === symbol(POWER) && equaln(car(cdr(car(cdr(cdr(p1))))), 2) && equalq(car(cdr(cdr(car(cdr(cdr(p1)))))), 1, 2)) {
            if (evaluatingAsFloats) {
              push_double(Math.PI * 3 / 4);
            } else {
              push_rational(3, 4);
              push_symbol(PI);
              multiply();
            }
            restore();
            return;
          }
          if (issqrtthreeovertwo(p1)) {
            if (evaluatingAsFloats) {
              push_double(Math.PI / 6);
            } else {
              push_rational(1, 6);
              push_symbol(PI);
              multiply();
            }
            restore();
            return;
          }
          if (isminussqrtthreeovertwo(p1)) {
            if (evaluatingAsFloats) {
              push_double(5 * Math.PI / 6);
            } else {
              push_rational(5, 6);
              push_symbol(PI);
              multiply();
            }
            restore();
            return;
          }
          if (!isrational(p1)) {
            push_symbol(ARCCOS);
            push(p1);
            list(2);
            restore();
            return;
          }
          push(p1);
          push_integer(2);
          multiply();
          n9 = pop_integer();
          switch (n9) {
            case -2:
              if (evaluatingAsFloats) {
                push_double(Math.PI);
              } else {
                push_symbol(PI);
              }
              break;
            case -1:
              if (evaluatingAsFloats) {
                push_double(Math.PI * 2 / 3);
              } else {
                push_rational(2, 3);
                push_symbol(PI);
                multiply();
              }
              break;
            case 0:
              if (evaluatingAsFloats) {
                push_double(Math.PI / 2);
              } else {
                push_rational(1, 2);
                push_symbol(PI);
                multiply();
              }
              break;
            case 1:
              if (evaluatingAsFloats) {
                push_double(Math.PI / 3);
              } else {
                push_rational(1, 3);
                push_symbol(PI);
                multiply();
              }
              break;
            case 2:
              if (evaluatingAsFloats) {
                push_double(0);
              } else {
                push(zero);
              }
              break;
            default:
              push_symbol(ARCCOS);
              push(p1);
              list(2);
          }
          return restore();
        };
        Eval_arccosh = function() {
          push(cadr(p1));
          Eval();
          return arccosh();
        };
        arccosh = function() {
          var d3;
          d3 = 0;
          save();
          p1 = pop();
          if (car(p1) === symbol(COSH)) {
            push(cadr(p1));
            restore();
            return;
          }
          if (isdouble(p1)) {
            d3 = p1.d;
            if (d3 < 1) {
              stop("arccosh function argument is less than 1.0");
            }
            d3 = Math.log(d3 + Math.sqrt(d3 * d3 - 1));
            push_double(d3);
            restore();
            return;
          }
          if (isplusone(p1)) {
            push(zero);
            restore();
            return;
          }
          push_symbol(ARCCOSH);
          push(p1);
          list(2);
          return restore();
        };
        Eval_arcsin = function() {
          push(cadr(p1));
          Eval();
          return arcsin();
        };
        arcsin = function() {
          var d3, errno, n9;
          n9 = 0;
          d3 = 0;
          save();
          p1 = pop();
          if (car(p1) === symbol(SIN)) {
            push(cadr(p1));
            restore();
            return;
          }
          if (isdouble(p1)) {
            errno = 0;
            d3 = Math.asin(p1.d);
            if (errno) {
              stop("arcsin function argument is not in the interval [-1,1]");
            }
            push_double(d3);
            restore();
            return;
          }
          if (isoneoversqrttwo(p1) || car(p1) === symbol(MULTIPLY) && equalq(car(cdr(p1)), 1, 2) && car(car(cdr(cdr(p1)))) === symbol(POWER) && equaln(car(cdr(car(cdr(cdr(p1))))), 2) && equalq(car(cdr(cdr(car(cdr(cdr(p1)))))), 1, 2)) {
            push_rational(1, 4);
            push_symbol(PI);
            multiply();
            restore();
            return;
          }
          if (isminusoneoversqrttwo(p1) || car(p1) === symbol(MULTIPLY) && equalq(car(cdr(p1)), -1, 2) && car(car(cdr(cdr(p1)))) === symbol(POWER) && equaln(car(cdr(car(cdr(cdr(p1))))), 2) && equalq(car(cdr(cdr(car(cdr(cdr(p1)))))), 1, 2)) {
            if (evaluatingAsFloats) {
              push_double(-Math.PI / 4);
            } else {
              push_rational(-1, 4);
              push_symbol(PI);
              multiply();
            }
            restore();
            return;
          }
          if (issqrtthreeovertwo(p1)) {
            if (evaluatingAsFloats) {
              push_double(Math.PI / 3);
            } else {
              push_rational(1, 3);
              push_symbol(PI);
              multiply();
            }
            restore();
            return;
          }
          if (isminussqrtthreeovertwo(p1)) {
            if (evaluatingAsFloats) {
              push_double(-Math.PI / 3);
            } else {
              push_rational(-1, 3);
              push_symbol(PI);
              multiply();
            }
            restore();
            return;
          }
          if (!isrational(p1)) {
            push_symbol(ARCSIN);
            push(p1);
            list(2);
            restore();
            return;
          }
          push(p1);
          push_integer(2);
          multiply();
          n9 = pop_integer();
          switch (n9) {
            case -2:
              if (evaluatingAsFloats) {
                push_double(-Math.PI / 2);
              } else {
                push_rational(-1, 2);
                push_symbol(PI);
                multiply();
              }
              break;
            case -1:
              if (evaluatingAsFloats) {
                push_double(-Math.PI / 6);
              } else {
                push_rational(-1, 6);
                push_symbol(PI);
                multiply();
              }
              break;
            case 0:
              if (evaluatingAsFloats) {
                push_double(0);
              } else {
                push(zero);
              }
              break;
            case 1:
              if (evaluatingAsFloats) {
                push_double(Math.PI / 6);
              } else {
                push_rational(1, 6);
                push_symbol(PI);
                multiply();
              }
              break;
            case 2:
              if (evaluatingAsFloats) {
                push_double(Math.PI / 2);
              } else {
                push_rational(1, 2);
                push_symbol(PI);
                multiply();
              }
              break;
            default:
              push_symbol(ARCSIN);
              push(p1);
              list(2);
          }
          return restore();
        };
        Eval_arcsinh = function() {
          push(cadr(p1));
          Eval();
          return arcsinh();
        };
        arcsinh = function() {
          var d3;
          d3 = 0;
          save();
          p1 = pop();
          if (car(p1) === symbol(SINH)) {
            push(cadr(p1));
            restore();
            return;
          }
          if (isdouble(p1)) {
            d3 = p1.d;
            d3 = Math.log(d3 + Math.sqrt(d3 * d3 + 1));
            push_double(d3);
            restore();
            return;
          }
          if (isZeroAtomOrTensor(p1)) {
            push(zero);
            restore();
            return;
          }
          push_symbol(ARCSINH);
          push(p1);
          list(2);
          return restore();
        };
        Eval_arctan = function() {
          push(cadr(p1));
          Eval();
          return arctan();
        };
        arctan = function() {
          var d3, errno;
          d3 = 0;
          save();
          p1 = pop();
          if (car(p1) === symbol(TAN)) {
            push(cadr(p1));
            restore();
            return;
          }
          if (isdouble(p1)) {
            errno = 0;
            d3 = Math.atan(p1.d);
            if (errno) {
              stop("arctan function error");
            }
            push_double(d3);
            restore();
            return;
          }
          if (isZeroAtomOrTensor(p1)) {
            push(zero);
            restore();
            return;
          }
          if (isnegative(p1)) {
            push(p1);
            negate();
            arctan();
            negate();
            restore();
            return;
          }
          if (Find(p1, symbol(SIN)) && Find(p1, symbol(COS))) {
            push(p1);
            numerator();
            p2 = pop();
            push(p1);
            denominator();
            p3 = pop();
            if (car(p2) === symbol(SIN) && car(p3) === symbol(COS) && equal(cadr(p2), cadr(p3))) {
              push(cadr(p2));
              restore();
              return;
            }
          }
          if (car(p1) === symbol(POWER) && equaln(cadr(p1), 3) && equalq(caddr(p1), -1, 2) || car(p1) === symbol(MULTIPLY) && equalq(car(cdr(p1)), 1, 3) && car(car(cdr(cdr(p1)))) === symbol(POWER) && equaln(car(cdr(car(cdr(cdr(p1))))), 3) && equalq(car(cdr(cdr(car(cdr(cdr(p1)))))), 1, 2)) {
            push_rational(1, 6);
            if (evaluatingAsFloats) {
              push_double(Math.PI);
            } else {
              push(symbol(PI));
            }
            multiply();
            restore();
            return;
          }
          if (equaln(p1, 1)) {
            push_rational(1, 4);
            if (evaluatingAsFloats) {
              push_double(Math.PI);
            } else {
              push(symbol(PI));
            }
            multiply();
            restore();
            return;
          }
          if (car(p1) === symbol(POWER) && equaln(cadr(p1), 3) && equalq(caddr(p1), 1, 2)) {
            push_rational(1, 3);
            if (evaluatingAsFloats) {
              push_double(Math.PI);
            } else {
              push(symbol(PI));
            }
            multiply();
            restore();
            return;
          }
          push_symbol(ARCTAN);
          push(p1);
          list(2);
          return restore();
        };
        Eval_arctanh = function() {
          push(cadr(p1));
          Eval();
          return arctanh();
        };
        arctanh = function() {
          var d3;
          d3 = 0;
          save();
          p1 = pop();
          if (car(p1) === symbol(TANH)) {
            push(cadr(p1));
            restore();
            return;
          }
          if (isdouble(p1)) {
            d3 = p1.d;
            if (d3 < -1 || d3 > 1) {
              stop("arctanh function argument is not in the interval [-1,1]");
            }
            d3 = Math.log((1 + d3) / (1 - d3)) / 2;
            push_double(d3);
            restore();
            return;
          }
          if (isZeroAtomOrTensor(p1)) {
            push(zero);
            restore();
            return;
          }
          push_symbol(ARCTANH);
          push(p1);
          list(2);
          return restore();
        };
        DEBUG_ARG = false;
        Eval_arg = function() {
          push(cadr(p1));
          Eval();
          return arg();
        };
        arg = function() {
          save();
          p1 = pop();
          push(p1);
          numerator();
          yyarg();
          push(p1);
          denominator();
          yyarg();
          subtract();
          return restore();
        };
        yyarg = function() {
          save();
          p1 = pop();
          if (ispositivenumber(p1) || p1 === symbol(PI)) {
            if (isdouble(p1) || evaluatingAsFloats) {
              push_double(0);
            } else {
              push_integer(0);
            }
          } else if (isnegativenumber(p1)) {
            if (isdouble(p1) || evaluatingAsFloats) {
              push_double(Math.PI);
            } else {
              push(symbol(PI));
            }
            negate();
          } else if (issymbol(p1)) {
            push_symbol(ARG);
            push(p1);
            list(2);
          } else if (car(p1) === symbol(POWER) && equaln(cadr(p1), -1)) {
            if (evaluatingAsFloats) {
              push_double(Math.PI);
            } else {
              push(symbol(PI));
            }
            push(caddr(p1));
            multiply();
          } else if (car(p1) === symbol(POWER) && cadr(p1) === symbol(E)) {
            push(caddr(p1));
            imag();
          } else if (car(p1) === symbol(POWER) && isoneovertwo(caddr(p1))) {
            if (DEBUG_ARG) {
              console.log("arg of a sqrt: " + p1);
            }
            if (DEBUG_ARG) {
              debugger;
            }
            push(cadr(p1));
            arg();
            if (DEBUG_ARG) {
              console.log(" = 1/2 * " + stack[tos - 1]);
            }
            push(caddr(p1));
            multiply();
          } else if (car(p1) === symbol(MULTIPLY)) {
            push_integer(0);
            p1 = cdr(p1);
            while (iscons(p1)) {
              push(car(p1));
              arg();
              add();
              p1 = cdr(p1);
            }
          } else if (car(p1) === symbol(ADD)) {
            push(p1);
            rect();
            p1 = pop();
            push(p1);
            real();
            p2 = pop();
            push(p1);
            imag();
            p3 = pop();
            if (isZeroAtomOrTensor(p2)) {
              if (evaluatingAsFloats) {
                push_double(Math.PI);
              } else {
                push(symbol(PI));
              }
              if (isnegative(p3)) {
                negate();
              }
            } else {
              push(p3);
              push(p2);
              divide();
              arctan();
              if (isnegative(p2)) {
                if (evaluatingAsFloats) {
                  push_double(Math.PI);
                } else {
                  push_symbol(PI);
                }
                if (isnegative(p3)) {
                  subtract();
                } else {
                  add();
                }
              }
            }
          } else {
            if (!isZeroAtomOrTensor(get_binding(symbol(ASSUME_REAL_VARIABLES)))) {
              push_integer(0);
            } else {
              push_symbol(ARG);
              push(p1);
              list(2);
            }
          }
          return restore();
        };
        bake = function() {
          var h5, s7, t5, x2, y2, z2;
          h5 = 0;
          s7 = 0;
          t5 = 0;
          x2 = 0;
          y2 = 0;
          z2 = 0;
          expanding++;
          save();
          p1 = pop();
          s7 = ispolyexpandedform(p1, symbol(SYMBOL_S));
          t5 = ispolyexpandedform(p1, symbol(SYMBOL_T));
          x2 = ispolyexpandedform(p1, symbol(SYMBOL_X));
          y2 = ispolyexpandedform(p1, symbol(SYMBOL_Y));
          z2 = ispolyexpandedform(p1, symbol(SYMBOL_Z));
          if (s7 === 1 && t5 === 0 && x2 === 0 && y2 === 0 && z2 === 0) {
            p2 = symbol(SYMBOL_S);
            bake_poly();
          } else if (s7 === 0 && t5 === 1 && x2 === 0 && y2 === 0 && z2 === 0) {
            p2 = symbol(SYMBOL_T);
            bake_poly();
          } else if (s7 === 0 && t5 === 0 && x2 === 1 && y2 === 0 && z2 === 0) {
            p2 = symbol(SYMBOL_X);
            bake_poly();
          } else if (s7 === 0 && t5 === 0 && x2 === 0 && y2 === 1 && z2 === 0) {
            p2 = symbol(SYMBOL_Y);
            bake_poly();
          } else if (s7 === 0 && t5 === 0 && x2 === 0 && y2 === 0 && z2 === 1) {
            p2 = symbol(SYMBOL_Z);
            bake_poly();
          } else if (iscons(p1) && car(p1) !== symbol(FOR)) {
            h5 = tos;
            push(car(p1));
            p1 = cdr(p1);
            while (iscons(p1)) {
              push(car(p1));
              bake();
              p1 = cdr(p1);
            }
            list(tos - h5);
          } else {
            push(p1);
          }
          restore();
          return expanding--;
        };
        polyform = function() {
          var h5;
          h5 = 0;
          save();
          p2 = pop();
          p1 = pop();
          if (ispolyexpandedform(p1, p2)) {
            bake_poly();
          } else if (iscons(p1)) {
            h5 = tos;
            push(car(p1));
            p1 = cdr(p1);
            while (iscons(p1)) {
              push(car(p1));
              push(p2);
              polyform();
              p1 = cdr(p1);
            }
            list(tos - h5);
          } else {
            push(p1);
          }
          return restore();
        };
        bake_poly = function() {
          var a4, h5, i5, k2, n9, o12, ref2;
          h5 = 0;
          i5 = 0;
          k2 = 0;
          n9 = 0;
          a4 = tos;
          k2 = coeff(p2, p1);
          h5 = tos;
          for (i5 = o12 = ref2 = k2 - 1; o12 >= 0; i5 = o12 += -1) {
            p1 = stack[a4 + i5];
            bake_poly_term(i5);
          }
          n9 = tos - h5;
          if (n9 > 1) {
            list(n9);
            push(symbol(ADD));
            swap();
            cons();
          }
          p1 = pop();
          moveTos(tos - k2);
          return push(p1);
        };
        bake_poly_term = function(k2) {
          var h5, n9;
          h5 = 0;
          n9 = 0;
          if (isZeroAtomOrTensor(p1)) {
            return;
          }
          if (k2 === 0) {
            if (car(p1) === symbol(ADD)) {
              p1 = cdr(p1);
              while (iscons(p1)) {
                push(car(p1));
                p1 = cdr(p1);
              }
            } else {
              push(p1);
            }
            return;
          }
          h5 = tos;
          if (car(p1) === symbol(MULTIPLY)) {
            p1 = cdr(p1);
            while (iscons(p1)) {
              push(car(p1));
              p1 = cdr(p1);
            }
          } else if (!equaln(p1, 1)) {
            push(p1);
          }
          if (k2 === 1) {
            push(p2);
          } else {
            push(symbol(POWER));
            push(p2);
            push_integer(k2);
            list(3);
          }
          n9 = tos - h5;
          if (n9 > 1) {
            list(n9);
            push(symbol(MULTIPLY));
            swap();
            return cons();
          }
        };
        Eval_besselj = function() {
          push(cadr(p1));
          Eval();
          push(caddr(p1));
          Eval();
          return besselj();
        };
        besselj = function() {
          save();
          yybesselj();
          return restore();
        };
        yybesselj = function() {
          var d3, n9;
          d3 = 0;
          n9 = 0;
          p2 = pop();
          p1 = pop();
          push(p2);
          n9 = pop_integer();
          if (isdouble(p1) && !isNaN(n9)) {
            d3 = jn(n9, p1.d);
            push_double(d3);
            return;
          }
          if (isZeroAtomOrTensor(p1) && isZeroAtomOrTensor(p2)) {
            push_integer(1);
            return;
          }
          if (isZeroAtomOrTensor(p1) && !isNaN(n9)) {
            push_integer(0);
            return;
          }
          if (p2.k === NUM && MEQUAL(p2.q.b, 2)) {
            if (MEQUAL(p2.q.a, 1)) {
              if (evaluatingAsFloats) {
                push_double(2 / Math.PI);
              } else {
                push_integer(2);
                push_symbol(PI);
                divide();
              }
              push(p1);
              divide();
              push_rational(1, 2);
              power();
              push(p1);
              sine();
              multiply();
              return;
            }
            if (MEQUAL(p2.q.a, -1)) {
              if (evaluatingAsFloats) {
                push_double(2 / Math.PI);
              } else {
                push_integer(2);
                push_symbol(PI);
                divide();
              }
              push(p1);
              divide();
              push_rational(1, 2);
              power();
              push(p1);
              cosine();
              multiply();
              return;
            }
            push_integer(MSIGN(p2.q.a));
            p3 = pop();
            push_integer(2);
            push(p1);
            divide();
            push(p2);
            push(p3);
            subtract();
            multiply();
            push(p1);
            push(p2);
            push(p3);
            subtract();
            besselj();
            multiply();
            push(p1);
            push(p2);
            push_integer(2);
            push(p3);
            multiply();
            subtract();
            besselj();
            subtract();
            return;
          }
          if (isnegativeterm(p1)) {
            push(p1);
            negate();
            push(p2);
            power();
            push(p1);
            push(p2);
            negate();
            power();
            multiply();
            push_symbol(BESSELJ);
            push(p1);
            negate();
            push(p2);
            list(3);
            multiply();
            return;
          }
          if (isnegativeterm(p2)) {
            push_integer(-1);
            push(p2);
            power();
            push_symbol(BESSELJ);
            push(p1);
            push(p2);
            negate();
            list(3);
            multiply();
            return;
          }
          push(symbol(BESSELJ));
          push(p1);
          push(p2);
          return list(3);
        };
        Eval_bessely = function() {
          push(cadr(p1));
          Eval();
          push(caddr(p1));
          Eval();
          return bessely();
        };
        bessely = function() {
          save();
          yybessely();
          return restore();
        };
        yybessely = function() {
          var d3, n9;
          d3 = 0;
          n9 = 0;
          p2 = pop();
          p1 = pop();
          push(p2);
          n9 = pop_integer();
          if (isdouble(p1) && !isNaN(n9)) {
            d3 = yn(n9, p1.d);
            push_double(d3);
            return;
          }
          if (isnegativeterm(p2)) {
            push_integer(-1);
            push(p2);
            power();
            push_symbol(BESSELY);
            push(p1);
            push(p2);
            negate();
            list(3);
            multiply();
            return;
          }
          push_symbol(BESSELY);
          push(p1);
          push(p2);
          list(3);
        };
        mint = function(a4) {
          return bigInt(a4);
        };
        isSmall = function(a4) {
          return a4.geq(Number.MIN_SAFE_INTEGER) && a4.leq(Number.MAX_SAFE_INTEGER);
        };
        setSignTo = function(a4, b2) {
          if (a4.isPositive()) {
            if (b2 < 0) {
              return a4.multiply(bigInt(-1));
            }
          } else {
            if (b2 > 0) {
              return a4.multiply(bigInt(-1));
            }
          }
          return a4;
        };
        makeSignSameAs = function(a4, b2) {
          if (a4.isPositive()) {
            if (b2.isNegative()) {
              return a4.multiply(bigInt(-1));
            }
          } else {
            if (b2.isPositive()) {
              return a4.multiply(bigInt(-1));
            }
          }
          return a4;
        };
        makePositive = function(a4) {
          if (a4.isNegative()) {
            return a4.multiply(bigInt(-1));
          }
          return a4;
        };
        add_numbers = function() {
          var a4, b2, theResult;
          a4 = 1;
          b2 = 1;
          if (isrational(stack[tos - 1]) && isrational(stack[tos - 2])) {
            qadd();
            return;
          }
          save();
          p2 = pop();
          p1 = pop();
          if (isdouble(p1)) {
            a4 = p1.d;
          } else {
            a4 = convert_rational_to_double(p1);
          }
          if (isdouble(p2)) {
            b2 = p2.d;
          } else {
            b2 = convert_rational_to_double(p2);
          }
          theResult = a4 + b2;
          push_double(theResult);
          return restore();
        };
        subtract_numbers = function() {
          var a4, b2;
          a4 = 0;
          b2 = 0;
          if (isrational(stack[tos - 1]) && isrational(stack[tos - 2])) {
            qsub();
            return;
          }
          save();
          p2 = pop();
          p1 = pop();
          if (isdouble(p1)) {
            a4 = p1.d;
          } else {
            a4 = convert_rational_to_double(p1);
          }
          if (isdouble(p2)) {
            b2 = p2.d;
          } else {
            b2 = convert_rational_to_double(p2);
          }
          push_double(a4 - b2);
          return restore();
        };
        multiply_numbers = function() {
          var a4, b2;
          a4 = 0;
          b2 = 0;
          if (isrational(stack[tos - 1]) && isrational(stack[tos - 2])) {
            qmul();
            return;
          }
          save();
          p2 = pop();
          p1 = pop();
          if (isdouble(p1)) {
            a4 = p1.d;
          } else {
            a4 = convert_rational_to_double(p1);
          }
          if (isdouble(p2)) {
            b2 = p2.d;
          } else {
            b2 = convert_rational_to_double(p2);
          }
          push_double(a4 * b2);
          return restore();
        };
        divide_numbers = function() {
          var a4, b2;
          a4 = 0;
          b2 = 0;
          if (isrational(stack[tos - 1]) && isrational(stack[tos - 2])) {
            qdiv();
            return;
          }
          save();
          p2 = pop();
          p1 = pop();
          if (isZeroAtomOrTensor(p2)) {
            stop("divide by zero");
          }
          if (isdouble(p1)) {
            a4 = p1.d;
          } else {
            a4 = convert_rational_to_double(p1);
          }
          if (isdouble(p2)) {
            b2 = p2.d;
          } else {
            b2 = convert_rational_to_double(p2);
          }
          push_double(a4 / b2);
          return restore();
        };
        invert_number = function() {
          var a4, b2;
          save();
          p1 = pop();
          if (isZeroAtomOrTensor(p1)) {
            stop("divide by zero");
          }
          if (isdouble(p1)) {
            push_double(1 / p1.d);
            restore();
            return;
          }
          a4 = bigInt(p1.q.a);
          b2 = bigInt(p1.q.b);
          b2 = makeSignSameAs(b2, a4);
          a4 = setSignTo(a4, 1);
          p1 = new U();
          p1.k = NUM;
          p1.q.a = b2;
          p1.q.b = a4;
          push(p1);
          return restore();
        };
        compare_rationals = function(a4, b2) {
          var ab, ba, t5;
          t5 = 0;
          ab = mmul(a4.q.a, b2.q.b);
          ba = mmul(a4.q.b, b2.q.a);
          t5 = mcmp(ab, ba);
          return t5;
        };
        compare_numbers = function(a4, b2) {
          var x2, y2;
          x2 = 0;
          y2 = 0;
          if (isrational(a4) && isrational(b2)) {
            return compare_rationals(a4, b2);
          }
          if (isdouble(a4)) {
            x2 = a4.d;
          } else {
            x2 = convert_rational_to_double(a4);
          }
          if (isdouble(b2)) {
            y2 = b2.d;
          } else {
            y2 = convert_rational_to_double(b2);
          }
          if (x2 < y2) {
            return -1;
          }
          if (x2 > y2) {
            return 1;
          }
          return 0;
        };
        negate_number = function() {
          save();
          p1 = pop();
          if (isZeroAtomOrTensor(p1)) {
            push(p1);
            restore();
            return;
          }
          switch (p1.k) {
            case NUM:
              p2 = new U();
              p2.k = NUM;
              p2.q.a = bigInt(p1.q.a.multiply(bigInt.minusOne));
              p2.q.b = bigInt(p1.q.b);
              push(p2);
              break;
            case DOUBLE:
              push_double(-p1.d);
              break;
            default:
              stop("bug caught in mp_negate_number");
          }
          return restore();
        };
        bignum_truncate = function() {
          var a4;
          save();
          p1 = pop();
          a4 = mdiv(p1.q.a, p1.q.b);
          p1 = new U();
          p1.k = NUM;
          p1.q.a = a4;
          p1.q.b = bigInt(1);
          push(p1);
          return restore();
        };
        mp_numerator = function() {
          save();
          p1 = pop();
          if (p1.k !== NUM) {
            push(one);
            restore();
            return;
          }
          p2 = new U();
          p2.k = NUM;
          p2.q.a = bigInt(p1.q.a);
          p2.q.b = bigInt(1);
          push(p2);
          return restore();
        };
        mp_denominator = function() {
          save();
          p1 = pop();
          if (p1.k !== NUM) {
            push(one);
            restore();
            return;
          }
          p2 = new U();
          p2.k = NUM;
          p2.q.a = bigInt(p1.q.b);
          p2.q.b = bigInt(1);
          push(p2);
          return restore();
        };
        bignum_power_number = function(expo) {
          var a4, b2, t5;
          save();
          p1 = pop();
          a4 = mpow(p1.q.a, Math.abs(expo));
          b2 = mpow(p1.q.b, Math.abs(expo));
          if (expo < 0) {
            t5 = a4;
            a4 = b2;
            b2 = t5;
            a4 = makeSignSameAs(a4, b2);
            b2 = setSignTo(b2, 1);
          }
          p1 = new U();
          p1.k = NUM;
          p1.q.a = a4;
          p1.q.b = b2;
          push(p1);
          return restore();
        };
        convert_bignum_to_double = function(p11) {
          return p11.toJSNumber();
        };
        convert_rational_to_double = function(p11) {
          var quotientAndRemainder, result;
          if (p11.q == null) {
            debugger;
          }
          quotientAndRemainder = p11.q.a.divmod(p11.q.b);
          result = quotientAndRemainder.quotient + quotientAndRemainder.remainder / p11.q.b.toJSNumber();
          return result;
        };
        new_integer = function(n9) {
          var theNewInteger;
          theNewInteger = new U();
          theNewInteger.k = NUM;
          theNewInteger.q.a = bigInt(n9);
          theNewInteger.q.b = bigInt(1);
          return theNewInteger;
        };
        push_integer = function(n9) {
          if (DEBUG) {
            console.log("pushing integer " + n9);
          }
          return push(new_integer(n9));
        };
        push_double = function(d3) {
          save();
          p1 = new U();
          p1.k = DOUBLE;
          p1.d = d3;
          push(p1);
          return restore();
        };
        push_rational = function(a4, b2) {
          var p11;
          p11 = new U();
          p11.k = NUM;
          p11.q.a = bigInt(a4);
          p11.q.b = bigInt(b2);
          return push(p11);
        };
        pop_integer = function() {
          var n9;
          n9 = 0 / 0;
          save();
          p1 = pop();
          switch (p1.k) {
            case NUM:
              if (isinteger(p1) && isSmall(p1.q.a)) {
                n9 = p1.q.a.toJSNumber();
              }
              break;
            case DOUBLE:
              if (DEBUG) {
                console.log("popping integer but double is found");
              }
              if (Math.floor(p1.d) === p1.d) {
                if (DEBUG) {
                  console.log("...altough it's an integer");
                }
                n9 = p1.d;
              }
          }
          restore();
          return n9;
        };
        print_double = function(p11, flag2) {
          var accumulator, buf;
          accumulator = "";
          buf = doubleToReasonableString(p11.d);
          if (flag2 === 1 && buf === "-") {
            accumulator += print_str(buf + 1);
          } else {
            accumulator += print_str(buf);
          }
          return accumulator;
        };
        bignum_scan_integer = function(s7) {
          var a4, scounter, sign_;
          save();
          scounter = 0;
          sign_ = s7[scounter];
          if (sign_ === "+" || sign_ === "-") {
            scounter++;
          }
          a4 = bigInt(s7.substring(scounter));
          p1 = new U();
          p1.k = NUM;
          p1.q.a = a4;
          p1.q.b = bigInt(1);
          push(p1);
          if (sign_ === "-") {
            negate();
          }
          return restore();
        };
        bignum_scan_float = function(s7) {
          return push_double(parseFloat(s7));
        };
        print_number = function(p11, signed) {
          var aAsString, accumulator, buf, denominatorString;
          accumulator = "";
          denominatorString = "";
          buf = "";
          switch (p11.k) {
            case NUM:
              aAsString = p11.q.a.toString();
              if (!signed) {
                if (aAsString[0] === "-") {
                  aAsString = aAsString.substring(1);
                }
              }
              if (printMode === PRINTMODE_LATEX && isfraction(p11)) {
                aAsString = "\\frac{" + aAsString + "}{";
              }
              accumulator += aAsString;
              if (isfraction(p11)) {
                if (printMode !== PRINTMODE_LATEX) {
                  accumulator += "/";
                }
                denominatorString = p11.q.b.toString();
                if (printMode === PRINTMODE_LATEX) {
                  denominatorString += "}";
                }
                accumulator += denominatorString;
              }
              break;
            case DOUBLE:
              aAsString = doubleToReasonableString(p11.d);
              if (!signed) {
                if (aAsString[0] === "-") {
                  aAsString = aAsString.substring(1);
                }
              }
              accumulator += aAsString;
          }
          return accumulator;
        };
        gcd_numbers = function() {
          save();
          p2 = pop();
          p1 = pop();
          p3 = new U();
          p3.k = NUM;
          p3.q.a = mgcd(p1.q.a, p2.q.a);
          p3.q.b = mgcd(p1.q.b, p2.q.b);
          p3.q.a = setSignTo(p3.q.a, 1);
          push(p3);
          return restore();
        };
        pop_double = function() {
          var d3;
          d3 = 0;
          save();
          p1 = pop();
          switch (p1.k) {
            case NUM:
              d3 = convert_rational_to_double(p1);
              break;
            case DOUBLE:
              d3 = p1.d;
              break;
            default:
              d3 = 0;
          }
          restore();
          return d3;
        };
        bignum_float = function() {
          var d3;
          d3 = 0;
          d3 = convert_rational_to_double(pop());
          return push_double(d3);
        };
        bignum_factorial = function(n9) {
          save();
          p1 = new U();
          p1.k = NUM;
          p1.q.a = __factorial(n9);
          p1.q.b = bigInt(1);
          push(p1);
          return restore();
        };
        __factorial = function(n9) {
          var a4, b2, i5, o12, ref2, t5;
          i5 = 0;
          if (n9 === 0 || n9 === 1) {
            a4 = bigInt(1);
            return a4;
          }
          a4 = bigInt(2);
          b2 = bigInt(0);
          if (3 <= n9) {
            for (i5 = o12 = 3, ref2 = n9; 3 <= ref2 ? o12 <= ref2 : o12 >= ref2; i5 = 3 <= ref2 ? ++o12 : --o12) {
              b2 = bigInt(i5);
              t5 = mmul(a4, b2);
              a4 = t5;
            }
          }
          return a4;
        };
        mask = [1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 131072, 262144, 524288, 1048576, 2097152, 4194304, 8388608, 16777216, 33554432, 67108864, 134217728, 268435456, 536870912, 1073741824, 2147483648];
        mp_set_bit = function(x2, k2) {
          console.log("not implemented yet");
          debugger;
          return x2[k2 / 32] |= mask[k2 % 32];
        };
        mp_clr_bit = function(x2, k2) {
          console.log("not implemented yet");
          debugger;
          return x2[k2 / 32] &= ~mask[k2 % 32];
        };
        mshiftright = function(a4) {
          return a4 = a4.shiftRight();
        };
        Eval_binomial = function() {
          push(cadr(p1));
          Eval();
          push(caddr(p1));
          Eval();
          return binomial();
        };
        binomial = function() {
          save();
          ybinomial();
          return restore();
        };
        ybinomial = function() {
          p2 = pop();
          p1 = pop();
          if (BINOM_check_args() === 0) {
            push(zero);
            return;
          }
          push(p1);
          factorial();
          push(p2);
          factorial();
          divide();
          push(p1);
          push(p2);
          subtract();
          factorial();
          return divide();
        };
        BINOM_check_args = function() {
          if (isNumericAtom(p1) && lessp(p1, zero)) {
            return 0;
          } else if (isNumericAtom(p2) && lessp(p2, zero)) {
            return 0;
          } else if (isNumericAtom(p1) && isNumericAtom(p2) && lessp(p1, p2)) {
            return 0;
          } else {
            return 1;
          }
        };
        Eval_ceiling = function() {
          push(cadr(p1));
          Eval();
          return ceiling();
        };
        ceiling = function() {
          save();
          yyceiling();
          return restore();
        };
        yyceiling = function() {
          var d3, doNothing;
          d3 = 0;
          p1 = pop();
          if (!isNumericAtom(p1)) {
            push_symbol(CEILING);
            push(p1);
            list(2);
            return;
          }
          if (isdouble(p1)) {
            d3 = Math.ceil(p1.d);
            push_double(d3);
            return;
          }
          if (isinteger(p1)) {
            push(p1);
            return;
          }
          p3 = new U();
          p3.k = NUM;
          p3.q.a = mdiv(p1.q.a, p1.q.b);
          p3.q.b = mint(1);
          push(p3);
          if (isnegativenumber(p1)) {
            return doNothing = 1;
          } else {
            push_integer(1);
            return add();
          }
        };
        Eval_choose = function() {
          push(cadr(p1));
          Eval();
          push(caddr(p1));
          Eval();
          return choose();
        };
        choose = function() {
          save();
          p2 = pop();
          p1 = pop();
          if (choose_check_args() === 0) {
            push_integer(0);
            restore();
            return;
          }
          push(p1);
          factorial();
          push(p2);
          factorial();
          divide();
          push(p1);
          push(p2);
          subtract();
          factorial();
          divide();
          return restore();
        };
        choose_check_args = function() {
          if (isNumericAtom(p1) && lessp(p1, zero)) {
            return 0;
          } else if (isNumericAtom(p2) && lessp(p2, zero)) {
            return 0;
          } else if (isNumericAtom(p1) && isNumericAtom(p2) && lessp(p1, p2)) {
            return 0;
          } else {
            return 1;
          }
        };
        Eval_circexp = function() {
          push(cadr(p1));
          Eval();
          circexp();
          return Eval();
        };
        circexp = function() {
          var h5, i5, o12, ref2;
          i5 = 0;
          h5 = 0;
          save();
          p1 = pop();
          if (car(p1) === symbol(COS)) {
            push(cadr(p1));
            expcos();
            restore();
            return;
          }
          if (car(p1) === symbol(SIN)) {
            push(cadr(p1));
            expsin();
            restore();
            return;
          }
          if (car(p1) === symbol(TAN)) {
            p1 = cadr(p1);
            push(imaginaryunit);
            push(p1);
            multiply();
            exponential();
            p2 = pop();
            push(imaginaryunit);
            push(p1);
            multiply();
            negate();
            exponential();
            p3 = pop();
            push(p3);
            push(p2);
            subtract();
            push(imaginaryunit);
            multiply();
            push(p2);
            push(p3);
            add();
            divide();
            restore();
            return;
          }
          if (car(p1) === symbol(COSH)) {
            p1 = cadr(p1);
            push(p1);
            exponential();
            push(p1);
            negate();
            exponential();
            add();
            push_rational(1, 2);
            multiply();
            restore();
            return;
          }
          if (car(p1) === symbol(SINH)) {
            p1 = cadr(p1);
            push(p1);
            exponential();
            push(p1);
            negate();
            exponential();
            subtract();
            push_rational(1, 2);
            multiply();
            restore();
            return;
          }
          if (car(p1) === symbol(TANH)) {
            p1 = cadr(p1);
            push(p1);
            push_integer(2);
            multiply();
            exponential();
            p1 = pop();
            push(p1);
            push_integer(1);
            subtract();
            push(p1);
            push_integer(1);
            add();
            divide();
            restore();
            return;
          }
          if (iscons(p1)) {
            h5 = tos;
            while (iscons(p1)) {
              push(car(p1));
              circexp();
              p1 = cdr(p1);
            }
            list(tos - h5);
            restore();
            return;
          }
          if (p1.k === TENSOR) {
            push(p1);
            copy_tensor();
            p1 = pop();
            for (i5 = o12 = 0, ref2 = p1.tensor.nelem; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
              push(p1.tensor.elem[i5]);
              circexp();
              p1.tensor.elem[i5] = pop();
            }
            push(p1);
            restore();
            return;
          }
          push(p1);
          return restore();
        };
        Eval_clearall = function() {
          do_clearall();
          return push(symbol(NIL));
        };
        do_clearall = function() {
          if (test_flag === 0) {
            clear_term();
          }
          do_clearPatterns();
          clear_symbols();
          defn();
          return codeGen = false;
        };
        clearall = function() {
          return run("clearall");
        };
        clearRenamedVariablesToAvoidBindingToExternalScope = function() {
          var i5, o12, ref2, results;
          results = [];
          for (i5 = o12 = 0, ref2 = symtab.length; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            if (symtab[i5].printname.indexOf("AVOID_BINDING_TO_EXTERNAL_SCOPE_VALUE") !== -1) {
              symtab[i5].k = SYM;
              symtab[i5].printname = "";
              binding[i5] = symtab[i5];
              results.push(isSymbolReclaimable[i5] = true);
            } else {
              results.push(void 0);
            }
          }
          return results;
        };
        Eval_clear = function() {
          var indexFound, variableToBeCleared;
          p2 = cdr(p1);
          while (iscons(p2)) {
            variableToBeCleared = car(p2);
            if (variableToBeCleared.k !== SYM) {
              stop("symbol error");
            }
            indexFound = symtab.indexOf(variableToBeCleared);
            symtab[indexFound].k = SYM;
            symtab[indexFound].printname = "";
            binding[indexFound] = symtab[indexFound];
            isSymbolReclaimable[indexFound] = true;
            p2 = cdr(p2);
          }
          return push(symbol(NIL));
        };
        DEBUG_CLOCKFORM = false;
        Eval_clock = function() {
          push(cadr(p1));
          Eval();
          return clockform();
        };
        clockform = function() {
          save();
          p1 = pop();
          push(p1);
          abs();
          if (DEBUG_CLOCKFORM) {
            console.log("clockform: abs of " + p1 + " : " + stack[tos - 1]);
          }
          push_symbol(POWER);
          push_integer(-1);
          push(p1);
          arg();
          if (DEBUG_CLOCKFORM) {
            console.log("clockform: arg of " + p1 + " : " + stack[tos - 1]);
          }
          if (evaluatingAsFloats) {
            push_double(Math.PI);
          } else {
            push(symbol(PI));
          }
          divide();
          if (DEBUG_CLOCKFORM) {
            console.log("clockform: divide : " + stack[tos - 1]);
          }
          list(3);
          if (DEBUG_CLOCKFORM) {
            console.log("clockform: power : " + stack[tos - 1]);
          }
          multiply();
          if (DEBUG_CLOCKFORM) {
            console.log("clockform: multiply : " + stack[tos - 1]);
          }
          return restore();
        };
        Eval_coeff = function() {
          push(cadr(p1));
          Eval();
          push(caddr(p1));
          Eval();
          push(cadddr(p1));
          Eval();
          p3 = pop();
          p2 = pop();
          p1 = pop();
          if (p3 === symbol(NIL)) {
            p3 = p2;
            p2 = symbol(SYMBOL_X);
          }
          push(p1);
          push(p2);
          push(p3);
          power();
          divide();
          push(p2);
          return filter();
        };
        coeff = function(variable, polynomial) {
          var coeffsCount, constant, polynomialWithoutConstant, prev_expanding;
          if (DEBUG) {
            console.log("coeff: " + variable + " " + polynomial);
          }
          coeffsCount = 0;
          while (true) {
            push(polynomial);
            push(variable);
            push(zero);
            subst();
            Eval();
            constant = pop();
            push(constant);
            coeffsCount++;
            push(polynomial);
            push(constant);
            subtract();
            polynomialWithoutConstant = pop();
            if (equal(polynomialWithoutConstant, zero)) {
              if (DEBUG) {
                console.log("coeff: result: " + coeffsCount);
              }
              return coeffsCount;
            }
            push(polynomialWithoutConstant);
            push(variable);
            prev_expanding = expanding;
            expanding = 1;
            divide();
            expanding = prev_expanding;
            polynomial = pop();
          }
        };
        Eval_cofactor = function() {
          var doNothing, i5, j2, n9;
          i5 = 0;
          j2 = 0;
          n9 = 0;
          push(cadr(p1));
          Eval();
          p2 = pop();
          if (istensor(p2) && p2.tensor.ndim === 2 && p2.tensor.dim[0] === p2.tensor.dim[1]) {
            doNothing = 1;
          } else {
            stop("cofactor: 1st arg: square matrix expected");
          }
          n9 = p2.tensor.dim[0];
          push(caddr(p1));
          Eval();
          i5 = pop_integer();
          if (i5 < 1 || i5 > n9) {
            stop("cofactor: 2nd arg: row index expected");
          }
          push(cadddr(p1));
          Eval();
          j2 = pop_integer();
          if (j2 < 1 || j2 > n9) {
            stop("cofactor: 3rd arg: column index expected");
          }
          return cofactor(p2, n9, i5 - 1, j2 - 1);
        };
        cofactor = function(p11, n9, row, col) {
          var i5, i12, j2, o12, ref2, ref12;
          i5 = 0;
          j2 = 0;
          for (i5 = o12 = 0, ref2 = n9; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            for (j2 = i12 = 0, ref12 = n9; 0 <= ref12 ? i12 < ref12 : i12 > ref12; j2 = 0 <= ref12 ? ++i12 : --i12) {
              if (i5 !== row && j2 !== col) {
                push(p11.tensor.elem[n9 * i5 + j2]);
              }
            }
          }
          determinant(n9 - 1);
          if ((row + col) % 2) {
            return negate();
          }
        };
        Eval_condense = function() {
          push(cadr(p1));
          Eval();
          return Condense();
        };
        Condense = function() {
          var prev_expanding;
          prev_expanding = expanding;
          expanding = 0;
          save();
          yycondense();
          restore();
          return expanding = prev_expanding;
        };
        yycondense = function() {
          p1 = pop();
          if (car(p1) !== symbol(ADD)) {
            push(p1);
            return;
          }
          p3 = cdr(p1);
          push(car(p3));
          p3 = cdr(p3);
          while (iscons(p3)) {
            push(car(p3));
            if (DEBUG) {
              console.log("calculating gcd between: " + stack[tos - 1] + " and " + stack[tos - 2]);
            }
            gcd();
            if (DEBUG) {
              console.log("partial gcd: " + stack[tos - 1]);
            }
            p3 = cdr(p3);
          }
          if (DEBUG) {
            console.log("condense: this is the gcd of all the terms: " + stack[tos - 1]);
          }
          inverse();
          p2 = pop();
          push(zero);
          p3 = cdr(p1);
          while (iscons(p3)) {
            push(p2);
            push(car(p3));
            multiply_noexpand();
            add();
            p3 = cdr(p3);
          }
          yyexpand();
          push(p2);
          return divide();
        };
        Eval_conj = function() {
          push(cadr(p1));
          Eval();
          p1 = pop();
          push(p1);
          if (!Find(p1, imaginaryunit)) {
            polar();
            conjugate();
            return clockform();
          } else {
            return conjugate();
          }
        };
        conjugate = function() {
          push(imaginaryunit);
          push(imaginaryunit);
          negate();
          subst();
          return Eval();
        };
        consCount = 0;
        cons = function() {
          var p11;
          consCount++;
          if (DEBUG) {
            console.log("cons tos: " + tos + " # " + consCount);
          }
          p11 = new U();
          p11.k = CONS;
          p11.cons.cdr = pop();
          if (p11 === p11.cons.cdr) {
            debugger;
            console.log("something wrong p == its cdr");
          }
          p11.cons.car = pop();
          return push(p11);
        };
        Eval_contract = function() {
          push(cadr(p1));
          Eval();
          if (cddr(p1) === symbol(NIL)) {
            push_integer(1);
            push_integer(2);
          } else {
            push(caddr(p1));
            Eval();
            push(cadddr(p1));
            Eval();
          }
          return contract();
        };
        contract = function() {
          save();
          yycontract();
          return restore();
        };
        yycontract = function() {
          var a4, ai, an, b2, h5, i5, i12, j2, j12, k2, l8, l1, m3, m1, n9, n1, ndim, nelem, o12, o1, ref2, ref12, ref22, ref3, ref4, ref5, ref6;
          h5 = 0;
          i5 = 0;
          j2 = 0;
          k2 = 0;
          l8 = 0;
          m3 = 0;
          n9 = 0;
          ndim = 0;
          nelem = 0;
          ai = [];
          an = [];
          p3 = pop();
          p2 = pop();
          p1 = pop();
          if (!istensor(p1)) {
            if (!isZeroAtomOrTensor(p1)) {
              stop("contract: tensor expected, 1st arg is not a tensor");
            }
            push(zero);
            return;
          }
          push(p2);
          l8 = pop_integer();
          push(p3);
          m3 = pop_integer();
          ndim = p1.tensor.ndim;
          if (l8 < 1 || l8 > ndim || m3 < 1 || m3 > ndim || l8 === m3 || p1.tensor.dim[l8 - 1] !== p1.tensor.dim[m3 - 1]) {
            stop("contract: index out of range");
          }
          l8--;
          m3--;
          n9 = p1.tensor.dim[l8];
          nelem = 1;
          for (i5 = o12 = 0, ref2 = ndim; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            if (i5 !== l8 && i5 !== m3) {
              nelem *= p1.tensor.dim[i5];
            }
          }
          p2 = alloc_tensor(nelem);
          p2.tensor.ndim = ndim - 2;
          j2 = 0;
          for (i5 = i12 = 0, ref12 = ndim; 0 <= ref12 ? i12 < ref12 : i12 > ref12; i5 = 0 <= ref12 ? ++i12 : --i12) {
            if (i5 !== l8 && i5 !== m3) {
              p2.tensor.dim[j2++] = p1.tensor.dim[i5];
            }
          }
          a4 = p1.tensor.elem;
          b2 = p2.tensor.elem;
          for (i5 = j12 = 0, ref22 = ndim; 0 <= ref22 ? j12 < ref22 : j12 > ref22; i5 = 0 <= ref22 ? ++j12 : --j12) {
            ai[i5] = 0;
            an[i5] = p1.tensor.dim[i5];
          }
          for (i5 = l1 = 0, ref3 = nelem; 0 <= ref3 ? l1 < ref3 : l1 > ref3; i5 = 0 <= ref3 ? ++l1 : --l1) {
            push(zero);
            for (j2 = m1 = 0, ref4 = n9; 0 <= ref4 ? m1 < ref4 : m1 > ref4; j2 = 0 <= ref4 ? ++m1 : --m1) {
              ai[l8] = j2;
              ai[m3] = j2;
              h5 = 0;
              for (k2 = n1 = 0, ref5 = ndim; 0 <= ref5 ? n1 < ref5 : n1 > ref5; k2 = 0 <= ref5 ? ++n1 : --n1) {
                h5 = h5 * an[k2] + ai[k2];
              }
              push(a4[h5]);
              add();
            }
            b2[i5] = pop();
            for (j2 = o1 = ref6 = ndim - 1; ref6 <= 0 ? o1 <= 0 : o1 >= 0; j2 = ref6 <= 0 ? ++o1 : --o1) {
              if (j2 === l8 || j2 === m3) {
                continue;
              }
              if (++ai[j2] < an[j2]) {
                break;
              }
              ai[j2] = 0;
            }
          }
          if (nelem === 1) {
            return push(b2[0]);
          } else {
            return push(p2);
          }
        };
        Eval_cos = function() {
          push(cadr(p1));
          Eval();
          return cosine();
        };
        cosine = function() {
          save();
          p1 = pop();
          if (car(p1) === symbol(ADD)) {
            cosine_of_angle_sum();
          } else {
            cosine_of_angle();
          }
          return restore();
        };
        cosine_of_angle_sum = function() {
          p2 = cdr(p1);
          while (iscons(p2)) {
            p4 = car(p2);
            if (isnpi(p4)) {
              push(p1);
              push(p4);
              subtract();
              p3 = pop();
              push(p3);
              cosine();
              push(p4);
              cosine();
              multiply();
              push(p3);
              sine();
              push(p4);
              sine();
              multiply();
              subtract();
              return;
            }
            p2 = cdr(p2);
          }
          return cosine_of_angle();
        };
        cosine_of_angle = function() {
          var d3, n9;
          if (car(p1) === symbol(ARCCOS)) {
            push(cadr(p1));
            return;
          }
          if (isdouble(p1)) {
            d3 = Math.cos(p1.d);
            if (Math.abs(d3) < 1e-10) {
              d3 = 0;
            }
            push_double(d3);
            return;
          }
          if (isnegative(p1)) {
            push(p1);
            negate();
            p1 = pop();
          }
          if (car(p1) === symbol(ARCTAN)) {
            push_integer(1);
            push(cadr(p1));
            push_integer(2);
            power();
            add();
            push_rational(-1, 2);
            power();
            return;
          }
          push(p1);
          push_integer(180);
          multiply();
          if (evaluatingAsFloats) {
            push_double(Math.PI);
          } else {
            push_symbol(PI);
          }
          divide();
          n9 = pop_integer();
          if (n9 < 0 || isNaN(n9)) {
            push(symbol(COS));
            push(p1);
            list(2);
            return;
          }
          switch (n9 % 360) {
            case 90:
            case 270:
              return push_integer(0);
            case 60:
            case 300:
              return push_rational(1, 2);
            case 120:
            case 240:
              return push_rational(-1, 2);
            case 45:
            case 315:
              push_rational(1, 2);
              push_integer(2);
              push_rational(1, 2);
              power();
              return multiply();
            case 135:
            case 225:
              push_rational(-1, 2);
              push_integer(2);
              push_rational(1, 2);
              power();
              return multiply();
            case 30:
            case 330:
              push_rational(1, 2);
              push_integer(3);
              push_rational(1, 2);
              power();
              return multiply();
            case 150:
            case 210:
              push_rational(-1, 2);
              push_integer(3);
              push_rational(1, 2);
              power();
              return multiply();
            case 0:
              return push_integer(1);
            case 180:
              return push_integer(-1);
            default:
              push(symbol(COS));
              push(p1);
              return list(2);
          }
        };
        Eval_cosh = function() {
          push(cadr(p1));
          Eval();
          return ycosh();
        };
        ycosh = function() {
          save();
          yycosh();
          return restore();
        };
        yycosh = function() {
          var d3;
          d3 = 0;
          p1 = pop();
          if (car(p1) === symbol(ARCCOSH)) {
            push(cadr(p1));
            return;
          }
          if (isdouble(p1)) {
            d3 = Math.cosh(p1.d);
            if (Math.abs(d3) < 1e-10) {
              d3 = 0;
            }
            push_double(d3);
            return;
          }
          if (isZeroAtomOrTensor(p1)) {
            push(one);
            return;
          }
          push_symbol(COSH);
          push(p1);
          return list(2);
        };
        Eval_decomp = function() {
          var h5;
          save();
          console.log("Eval_decomp is being called!!!!!!!!!!!!!!!!!!!!");
          h5 = tos;
          push(symbol(NIL));
          push(cadr(p1));
          Eval();
          push(caddr(p1));
          Eval();
          p1 = pop();
          if (p1 === symbol(NIL)) {
            guess();
          } else {
            push(p1);
          }
          decomp(false);
          list(tos - h5);
          return restore();
        };
        pushTryNotToDuplicate = function(toBePushed) {
          if (tos > 0) {
            if (DEBUG) {
              console.log("comparing " + toBePushed + " to: " + stack[tos - 1]);
            }
            if (equal(toBePushed, stack[tos - 1])) {
              if (DEBUG) {
                console.log("skipping " + toBePushed + " because it's already on stack ");
              }
              return;
            }
          }
          return push(toBePushed);
        };
        decomp = function(generalTransform) {
          save();
          p2 = pop();
          p1 = pop();
          if (DEBUG) {
            console.log("DECOMPOSING " + p1);
          }
          if (generalTransform) {
            if (!iscons(p1)) {
              if (DEBUG) {
                console.log(" ground thing: " + p1);
              }
              pushTryNotToDuplicate(p1);
              restore();
              return;
            }
          } else {
            if (Find(p1, p2) === 0) {
              if (DEBUG) {
                console.log(" entire expression is constant");
              }
              pushTryNotToDuplicate(p1);
              restore();
              return;
            }
          }
          if (isadd(p1)) {
            decomp_sum(generalTransform);
            restore();
            return;
          }
          if (ismultiply(p1)) {
            decomp_product(generalTransform);
            restore();
            return;
          }
          if (DEBUG) {
            console.log(" naive decomp");
          }
          p3 = cdr(p1);
          if (DEBUG) {
            console.log("startig p3: " + p3);
          }
          while (iscons(p3)) {
            if (generalTransform) {
              push(car(p3));
            }
            if (DEBUG) {
              console.log("recursive decomposition");
            }
            push(car(p3));
            if (DEBUG) {
              console.log("car(p3): " + car(p3));
            }
            push(p2);
            if (DEBUG) {
              console.log("p2: " + p2);
            }
            decomp(generalTransform);
            p3 = cdr(p3);
          }
          return restore();
        };
        decomp_sum = function(generalTransform) {
          var h5;
          if (DEBUG) {
            console.log(" decomposing the sum ");
          }
          h5 = 0;
          p3 = cdr(p1);
          while (iscons(p3)) {
            if (Find(car(p3), p2) || generalTransform) {
              push(car(p3));
              push(p2);
              decomp(generalTransform);
            }
            p3 = cdr(p3);
          }
          h5 = tos;
          p3 = cdr(p1);
          while (iscons(p3)) {
            if (Find(car(p3), p2) === 0) {
              pushTryNotToDuplicate(car(p3));
            }
            p3 = cdr(p3);
          }
          if (tos - h5) {
            add_all(tos - h5);
            p3 = pop();
            pushTryNotToDuplicate(p3);
            push(p3);
            return negate();
          }
        };
        decomp_product = function(generalTransform) {
          var h5;
          if (DEBUG) {
            console.log(" decomposing the product ");
          }
          h5 = 0;
          p3 = cdr(p1);
          while (iscons(p3)) {
            if (Find(car(p3), p2) || generalTransform) {
              push(car(p3));
              push(p2);
              decomp(generalTransform);
            }
            p3 = cdr(p3);
          }
          h5 = tos;
          p3 = cdr(p1);
          while (iscons(p3)) {
            if (Find(car(p3), p2) === 0) {
              pushTryNotToDuplicate(car(p3));
            }
            p3 = cdr(p3);
          }
          if (tos - h5) {
            return multiply_all(tos - h5);
          }
        };
        define_user_function = function() {
          p3 = caadr(p1);
          p4 = cdadr(p1);
          p5 = caddr(p1);
          if (!issymbol(p3)) {
            stop("function name?");
          }
          if (car(p5) === symbol(EVAL)) {
            push(cadr(p5));
            Eval();
            p5 = pop();
          }
          push_symbol(FUNCTION);
          push(p5);
          push(p4);
          list(3);
          p5 = pop();
          set_binding(p3, p5);
          return push_symbol(NIL);
        };
        Eval_function_reference = function() {
          return push(p1);
        };
        Eval_defint = function() {
          push(cadr(p1));
          Eval();
          p2 = pop();
          p1 = cddr(p1);
          while (iscons(p1)) {
            push(car(p1));
            p1 = cdr(p1);
            Eval();
            p3 = pop();
            push(car(p1));
            p1 = cdr(p1);
            Eval();
            p4 = pop();
            push(car(p1));
            p1 = cdr(p1);
            Eval();
            p5 = pop();
            push(p2);
            push(p3);
            integral();
            p2 = pop();
            push(p2);
            push(p3);
            push(p5);
            subst();
            Eval();
            push(p2);
            push(p3);
            push(p4);
            subst();
            Eval();
            subtract();
            p2 = pop();
          }
          return push(p2);
        };
        Eval_degree = function() {
          push(cadr(p1));
          Eval();
          push(caddr(p1));
          Eval();
          p1 = pop();
          if (p1 === symbol(NIL)) {
            guess();
          } else {
            push(p1);
          }
          return degree();
        };
        degree = function() {
          save();
          p2 = pop();
          p1 = pop();
          p3 = zero;
          yydegree(p1);
          push(p3);
          return restore();
        };
        yydegree = function(p11) {
          var results;
          if (equal(p11, p2)) {
            if (isZeroAtomOrTensor(p3)) {
              return p3 = one;
            }
          } else if (car(p11) === symbol(POWER)) {
            if (equal(cadr(p11), p2) && isNumericAtom(caddr(p11)) && lessp(p3, caddr(p11))) {
              return p3 = caddr(p11);
            }
          } else if (iscons(p11)) {
            p11 = cdr(p11);
            results = [];
            while (iscons(p11)) {
              yydegree(car(p11));
              results.push(p11 = cdr(p11));
            }
            return results;
          }
        };
        Eval_denominator = function() {
          push(cadr(p1));
          Eval();
          return denominator();
        };
        denominator = function() {
          var h5, theArgument2;
          h5 = 0;
          theArgument2 = pop();
          if (car(theArgument2) === symbol(ADD)) {
            push(theArgument2);
            rationalize();
            theArgument2 = pop();
          }
          if (car(theArgument2) === symbol(MULTIPLY) && !isplusone(car(cdr(theArgument2)))) {
            h5 = tos;
            theArgument2 = cdr(theArgument2);
            while (iscons(theArgument2)) {
              push(car(theArgument2));
              denominator();
              theArgument2 = cdr(theArgument2);
            }
            return multiply_all(tos - h5);
          } else if (isrational(theArgument2)) {
            push(theArgument2);
            return mp_denominator();
          } else if (car(theArgument2) === symbol(POWER) && isnegativeterm(caddr(theArgument2))) {
            push(theArgument2);
            return reciprocate();
          } else {
            return push(one);
          }
        };
        Eval_derivative = function() {
          var doNothing, i5, i12, n9, o12, ref2, ref12;
          i5 = 0;
          p1 = cdr(p1);
          push(car(p1));
          Eval();
          p1 = cdr(p1);
          push(car(p1));
          Eval();
          p2 = pop();
          if (p2 === symbol(NIL)) {
            guess();
            push(symbol(NIL));
          } else if (isNumericAtom(p2)) {
            guess();
            push(p2);
          } else {
            push(p2);
            p1 = cdr(p1);
            push(car(p1));
            Eval();
          }
          p5 = pop();
          p4 = pop();
          p3 = pop();
          while (1) {
            if (isNumericAtom(p5)) {
              push(p5);
              n9 = pop_integer();
              if (isNaN(n9)) {
                stop("nth derivative: check n");
              }
            } else {
              n9 = 1;
            }
            push(p3);
            if (n9 >= 0) {
              for (i5 = o12 = 0, ref2 = n9; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
                push(p4);
                derivative();
              }
            } else {
              n9 = -n9;
              for (i5 = i12 = 0, ref12 = n9; 0 <= ref12 ? i12 < ref12 : i12 > ref12; i5 = 0 <= ref12 ? ++i12 : --i12) {
                push(p4);
                integral();
              }
            }
            p3 = pop();
            if (p5 === symbol(NIL)) {
              break;
            }
            if (isNumericAtom(p5)) {
              p1 = cdr(p1);
              push(car(p1));
              Eval();
              p5 = pop();
              if (p5 === symbol(NIL)) {
                break;
              }
              if (isNumericAtom(p5)) {
                doNothing = 1;
              } else {
                p4 = p5;
                p1 = cdr(p1);
                push(car(p1));
                Eval();
                p5 = pop();
              }
            } else {
              p4 = p5;
              p1 = cdr(p1);
              push(car(p1));
              Eval();
              p5 = pop();
            }
          }
          return push(p3);
        };
        derivative = function() {
          save();
          p2 = pop();
          p1 = pop();
          if (isNumericAtom(p2)) {
            stop("undefined function");
          }
          if (istensor(p1)) {
            if (istensor(p2)) {
              d_tensor_tensor();
            } else {
              d_tensor_scalar();
            }
          } else {
            if (istensor(p2)) {
              d_scalar_tensor();
            } else {
              d_scalar_scalar();
            }
          }
          return restore();
        };
        d_scalar_scalar = function() {
          if (issymbol(p2)) {
            return d_scalar_scalar_1();
          } else {
            push(p1);
            push(p2);
            push(symbol(SECRETX));
            subst();
            push(symbol(SECRETX));
            derivative();
            push(symbol(SECRETX));
            push(p2);
            return subst();
          }
        };
        d_scalar_scalar_1 = function() {
          if (equal(p1, p2)) {
            push(one);
            return;
          }
          if (!iscons(p1)) {
            push(zero);
            return;
          }
          if (isadd(p1)) {
            dsum();
            return;
          }
          if (car(p1) === symbol(MULTIPLY)) {
            dproduct();
            return;
          }
          if (car(p1) === symbol(POWER)) {
            dpower();
            return;
          }
          if (car(p1) === symbol(DERIVATIVE)) {
            dd();
            return;
          }
          if (car(p1) === symbol(LOG)) {
            dlog();
            return;
          }
          if (car(p1) === symbol(SIN)) {
            dsin();
            return;
          }
          if (car(p1) === symbol(COS)) {
            dcos();
            return;
          }
          if (car(p1) === symbol(TAN)) {
            dtan();
            return;
          }
          if (car(p1) === symbol(ARCSIN)) {
            darcsin();
            return;
          }
          if (car(p1) === symbol(ARCCOS)) {
            darccos();
            return;
          }
          if (car(p1) === symbol(ARCTAN)) {
            darctan();
            return;
          }
          if (car(p1) === symbol(SINH)) {
            dsinh();
            return;
          }
          if (car(p1) === symbol(COSH)) {
            dcosh();
            return;
          }
          if (car(p1) === symbol(TANH)) {
            dtanh();
            return;
          }
          if (car(p1) === symbol(ARCSINH)) {
            darcsinh();
            return;
          }
          if (car(p1) === symbol(ARCCOSH)) {
            darccosh();
            return;
          }
          if (car(p1) === symbol(ARCTANH)) {
            darctanh();
            return;
          }
          if (car(p1) === symbol(ABS)) {
            dabs();
            return;
          }
          if (car(p1) === symbol(SGN)) {
            dsgn();
            return;
          }
          if (car(p1) === symbol(HERMITE)) {
            dhermite();
            return;
          }
          if (car(p1) === symbol(ERF)) {
            derf();
            return;
          }
          if (car(p1) === symbol(ERFC)) {
            derfc();
            return;
          }
          if (car(p1) === symbol(BESSELJ)) {
            if (isZeroAtomOrTensor(caddr(p1))) {
              dbesselj0();
            } else {
              dbesseljn();
            }
            return;
          }
          if (car(p1) === symbol(BESSELY)) {
            if (isZeroAtomOrTensor(caddr(p1))) {
              dbessely0();
            } else {
              dbesselyn();
            }
            return;
          }
          if (car(p1) === symbol(INTEGRAL) && caddr(p1) === p2) {
            derivative_of_integral();
            return;
          }
          return dfunction();
        };
        dsum = function() {
          var h5;
          h5 = tos;
          p1 = cdr(p1);
          while (iscons(p1)) {
            push(car(p1));
            push(p2);
            derivative();
            p1 = cdr(p1);
          }
          return add_all(tos - h5);
        };
        dproduct = function() {
          var i5, i12, j2, n9, o12, ref2, ref12;
          i5 = 0;
          j2 = 0;
          n9 = 0;
          n9 = length(p1) - 1;
          for (i5 = o12 = 0, ref2 = n9; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            p3 = cdr(p1);
            for (j2 = i12 = 0, ref12 = n9; 0 <= ref12 ? i12 < ref12 : i12 > ref12; j2 = 0 <= ref12 ? ++i12 : --i12) {
              push(car(p3));
              if (i5 === j2) {
                push(p2);
                derivative();
              }
              p3 = cdr(p3);
            }
            multiply_all(n9);
          }
          return add_all(n9);
        };
        dpower = function() {
          push(caddr(p1));
          push(cadr(p1));
          divide();
          push(cadr(p1));
          push(p2);
          derivative();
          multiply();
          push(cadr(p1));
          logarithm();
          push(caddr(p1));
          push(p2);
          derivative();
          multiply();
          add();
          push(p1);
          return multiply();
        };
        dlog = function() {
          push(cadr(p1));
          push(p2);
          derivative();
          push(cadr(p1));
          return divide();
        };
        dd = function() {
          push(cadr(p1));
          push(p2);
          derivative();
          p3 = pop();
          if (car(p3) === symbol(DERIVATIVE)) {
            push_symbol(DERIVATIVE);
            push_symbol(DERIVATIVE);
            push(cadr(p3));
            if (lessp(caddr(p3), caddr(p1))) {
              push(caddr(p3));
              list(3);
              push(caddr(p1));
            } else {
              push(caddr(p1));
              list(3);
              push(caddr(p3));
            }
            return list(3);
          } else {
            push(p3);
            push(caddr(p1));
            return derivative();
          }
        };
        dfunction = function() {
          p3 = cdr(p1);
          if (p3 === symbol(NIL) || Find(p3, p2)) {
            push_symbol(DERIVATIVE);
            push(p1);
            push(p2);
            return list(3);
          } else {
            return push(zero);
          }
        };
        dsin = function() {
          push(cadr(p1));
          push(p2);
          derivative();
          push(cadr(p1));
          cosine();
          return multiply();
        };
        dcos = function() {
          push(cadr(p1));
          push(p2);
          derivative();
          push(cadr(p1));
          sine();
          multiply();
          return negate();
        };
        dtan = function() {
          push(cadr(p1));
          push(p2);
          derivative();
          push(cadr(p1));
          cosine();
          push_integer(-2);
          power();
          return multiply();
        };
        darcsin = function() {
          push(cadr(p1));
          push(p2);
          derivative();
          push_integer(1);
          push(cadr(p1));
          push_integer(2);
          power();
          subtract();
          push_rational(-1, 2);
          power();
          return multiply();
        };
        darccos = function() {
          push(cadr(p1));
          push(p2);
          derivative();
          push_integer(1);
          push(cadr(p1));
          push_integer(2);
          power();
          subtract();
          push_rational(-1, 2);
          power();
          multiply();
          return negate();
        };
        darctan = function() {
          push(cadr(p1));
          push(p2);
          derivative();
          push_integer(1);
          push(cadr(p1));
          push_integer(2);
          power();
          add();
          inverse();
          multiply();
          return simplify();
        };
        dsinh = function() {
          push(cadr(p1));
          push(p2);
          derivative();
          push(cadr(p1));
          ycosh();
          return multiply();
        };
        dcosh = function() {
          push(cadr(p1));
          push(p2);
          derivative();
          push(cadr(p1));
          ysinh();
          return multiply();
        };
        dtanh = function() {
          push(cadr(p1));
          push(p2);
          derivative();
          push(cadr(p1));
          ycosh();
          push_integer(-2);
          power();
          return multiply();
        };
        darcsinh = function() {
          push(cadr(p1));
          push(p2);
          derivative();
          push(cadr(p1));
          push_integer(2);
          power();
          push_integer(1);
          add();
          push_rational(-1, 2);
          power();
          return multiply();
        };
        darccosh = function() {
          push(cadr(p1));
          push(p2);
          derivative();
          push(cadr(p1));
          push_integer(2);
          power();
          push_integer(-1);
          add();
          push_rational(-1, 2);
          power();
          return multiply();
        };
        darctanh = function() {
          push(cadr(p1));
          push(p2);
          derivative();
          push_integer(1);
          push(cadr(p1));
          push_integer(2);
          power();
          subtract();
          inverse();
          return multiply();
        };
        dabs = function() {
          push(cadr(p1));
          push(p2);
          derivative();
          push(cadr(p1));
          sgn();
          return multiply();
        };
        dsgn = function() {
          push(cadr(p1));
          push(p2);
          derivative();
          push(cadr(p1));
          dirac();
          multiply();
          push_integer(2);
          return multiply();
        };
        dhermite = function() {
          push(cadr(p1));
          push(p2);
          derivative();
          push_integer(2);
          push(caddr(p1));
          multiply();
          multiply();
          push(cadr(p1));
          push(caddr(p1));
          push_integer(-1);
          add();
          hermite();
          return multiply();
        };
        derf = function() {
          push(cadr(p1));
          push_integer(2);
          power();
          push_integer(-1);
          multiply();
          exponential();
          if (evaluatingAsFloats) {
            push_double(Math.PI);
          } else {
            push_symbol(PI);
          }
          push_rational(-1, 2);
          power();
          multiply();
          push_integer(2);
          multiply();
          push(cadr(p1));
          push(p2);
          derivative();
          return multiply();
        };
        derfc = function() {
          push(cadr(p1));
          push_integer(2);
          power();
          push_integer(-1);
          multiply();
          exponential();
          if (evaluatingAsFloats) {
            push_double(Math.PI);
          } else {
            push_symbol(PI);
          }
          push_rational(-1, 2);
          power();
          multiply();
          push_integer(-2);
          multiply();
          push(cadr(p1));
          push(p2);
          derivative();
          return multiply();
        };
        dbesselj0 = function() {
          push(cadr(p1));
          push(p2);
          derivative();
          push(cadr(p1));
          push_integer(1);
          besselj();
          multiply();
          push_integer(-1);
          return multiply();
        };
        dbesseljn = function() {
          push(cadr(p1));
          push(p2);
          derivative();
          push(cadr(p1));
          push(caddr(p1));
          push_integer(-1);
          add();
          besselj();
          push(caddr(p1));
          push_integer(-1);
          multiply();
          push(cadr(p1));
          divide();
          push(cadr(p1));
          push(caddr(p1));
          besselj();
          multiply();
          add();
          return multiply();
        };
        dbessely0 = function() {
          push(cadr(p1));
          push(p2);
          derivative();
          push(cadr(p1));
          push_integer(1);
          besselj();
          multiply();
          push_integer(-1);
          return multiply();
        };
        dbesselyn = function() {
          push(cadr(p1));
          push(p2);
          derivative();
          push(cadr(p1));
          push(caddr(p1));
          push_integer(-1);
          add();
          bessely();
          push(caddr(p1));
          push_integer(-1);
          multiply();
          push(cadr(p1));
          divide();
          push(cadr(p1));
          push(caddr(p1));
          bessely();
          multiply();
          add();
          return multiply();
        };
        derivative_of_integral = function() {
          return push(cadr(p1));
        };
        DET_check_arg = function() {
          if (!istensor(p1)) {
            return 0;
          } else if (p1.tensor.ndim !== 2) {
            return 0;
          } else if (p1.tensor.dim[0] !== p1.tensor.dim[1]) {
            return 0;
          } else {
            return 1;
          }
        };
        det = function() {
          var a4, i5, i12, n9, o12, ref2, ref12;
          i5 = 0;
          n9 = 0;
          save();
          p1 = pop();
          if (DET_check_arg() === 0) {
            push_symbol(DET);
            push(p1);
            list(2);
            restore();
            return;
          }
          n9 = p1.tensor.nelem;
          a4 = p1.tensor.elem;
          for (i5 = o12 = 0, ref2 = n9; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            if (!isNumericAtom(a4[i5])) {
              break;
            }
          }
          if (i5 === n9) {
            yydetg();
          } else {
            for (i5 = i12 = 0, ref12 = p1.tensor.nelem; 0 <= ref12 ? i12 < ref12 : i12 > ref12; i5 = 0 <= ref12 ? ++i12 : --i12) {
              push(p1.tensor.elem[i5]);
            }
            determinant(p1.tensor.dim[0]);
          }
          return restore();
        };
        determinant = function(n9) {
          var a4, breakFromOutherWhile, h5, i5, i12, j2, k2, o12, q, ref2, ref12, s7, sign_, t5;
          h5 = 0;
          i5 = 0;
          j2 = 0;
          k2 = 0;
          q = 0;
          s7 = 0;
          sign_ = 0;
          t5 = 0;
          a4 = [];
          h5 = tos - n9 * n9;
          for (i5 = o12 = 0, ref2 = n9; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            a4[i5] = i5;
            a4[i5 + n9] = 0;
            a4[i5 + n9 + n9] = 1;
          }
          sign_ = 1;
          push(zero);
          while (1) {
            if (sign_ === 1) {
              push_integer(1);
            } else {
              push_integer(-1);
            }
            for (i5 = i12 = 0, ref12 = n9; 0 <= ref12 ? i12 < ref12 : i12 > ref12; i5 = 0 <= ref12 ? ++i12 : --i12) {
              k2 = n9 * a4[i5] + i5;
              push(stack[h5 + k2]);
              multiply();
            }
            add();
            j2 = n9 - 1;
            s7 = 0;
            breakFromOutherWhile = false;
            while (1) {
              q = a4[n9 + j2] + a4[n9 + n9 + j2];
              if (q < 0) {
                a4[n9 + n9 + j2] = -a4[n9 + n9 + j2];
                j2--;
                continue;
              }
              if (q === j2 + 1) {
                if (j2 === 0) {
                  breakFromOutherWhile = true;
                  break;
                }
                s7++;
                a4[n9 + n9 + j2] = -a4[n9 + n9 + j2];
                j2--;
                continue;
              }
              break;
            }
            if (breakFromOutherWhile) {
              break;
            }
            t5 = a4[j2 - a4[n9 + j2] + s7];
            a4[j2 - a4[n9 + j2] + s7] = a4[j2 - q + s7];
            a4[j2 - q + s7] = t5;
            a4[n9 + j2] = q;
            sign_ = -sign_;
          }
          stack[h5] = stack[tos - 1];
          return moveTos(h5 + 1);
        };
        detg = function() {
          save();
          p1 = pop();
          if (DET_check_arg() === 0) {
            push_symbol(DET);
            push(p1);
            list(2);
            restore();
            return;
          }
          yydetg();
          return restore();
        };
        yydetg = function() {
          var i5, n9, o12, ref2;
          i5 = 0;
          n9 = 0;
          n9 = p1.tensor.dim[0];
          for (i5 = o12 = 0, ref2 = n9 * n9; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            push(p1.tensor.elem[i5]);
          }
          lu_decomp(n9);
          moveTos(tos - n9 * n9);
          return push(p1);
        };
        M = function(h5, n9, i5, j2) {
          return stack[h5 + n9 * i5 + j2];
        };
        setM = function(h5, n9, i5, j2, value) {
          return stack[h5 + n9 * i5 + j2] = value;
        };
        lu_decomp = function(n9) {
          var d3, h5, i5, i12, j2, j12, l1, m1, o12, ref2, ref12, ref22, ref3, ref4, ref5, ref6, ref7, ref8;
          d3 = 0;
          h5 = 0;
          i5 = 0;
          j2 = 0;
          h5 = tos - n9 * n9;
          p1 = one;
          for (d3 = o12 = 0, ref2 = n9 - 1; 0 <= ref2 ? o12 < ref2 : o12 > ref2; d3 = 0 <= ref2 ? ++o12 : --o12) {
            if (equal(M(h5, n9, d3, d3), zero)) {
              for (i5 = i12 = ref12 = d3 + 1, ref22 = n9; ref12 <= ref22 ? i12 < ref22 : i12 > ref22; i5 = ref12 <= ref22 ? ++i12 : --i12) {
                if (!equal(M(h5, n9, i5, d3), zero)) {
                  break;
                }
              }
              if (i5 === n9) {
                p1 = zero;
                break;
              }
              for (j2 = j12 = ref3 = d3, ref4 = n9; ref3 <= ref4 ? j12 < ref4 : j12 > ref4; j2 = ref3 <= ref4 ? ++j12 : --j12) {
                p2 = M(h5, n9, d3, j2);
                setM(h5, n9, d3, j2, M(h5, n9, i5, j2));
                setM(h5, n9, i5, j2, p2);
              }
              push(p1);
              negate();
              p1 = pop();
            }
            push(p1);
            push(M(h5, n9, d3, d3));
            multiply();
            p1 = pop();
            for (i5 = l1 = ref5 = d3 + 1, ref6 = n9; ref5 <= ref6 ? l1 < ref6 : l1 > ref6; i5 = ref5 <= ref6 ? ++l1 : --l1) {
              push(M(h5, n9, i5, d3));
              push(M(h5, n9, d3, d3));
              divide();
              negate();
              p2 = pop();
              setM(h5, n9, i5, d3, zero);
              for (j2 = m1 = ref7 = d3 + 1, ref8 = n9; ref7 <= ref8 ? m1 < ref8 : m1 > ref8; j2 = ref7 <= ref8 ? ++m1 : --m1) {
                push(M(h5, n9, d3, j2));
                push(p2);
                multiply();
                push(M(h5, n9, i5, j2));
                add();
                setM(h5, n9, i5, j2, pop());
              }
            }
          }
          push(p1);
          push(M(h5, n9, n9 - 1, n9 - 1));
          multiply();
          return p1 = pop();
        };
        Eval_dirac = function() {
          push(cadr(p1));
          Eval();
          return dirac();
        };
        dirac = function() {
          save();
          ydirac();
          return restore();
        };
        ydirac = function() {
          p1 = pop();
          if (isdouble(p1)) {
            if (p1.d === 0) {
              push_integer(1);
              return;
            } else {
              push_integer(0);
              return;
            }
          }
          if (isrational(p1)) {
            if (MZERO(mmul(p1.q.a, p1.q.b))) {
              push_integer(1);
              return;
            } else {
              push_integer(0);
              return;
            }
          }
          if (car(p1) === symbol(POWER)) {
            push_symbol(DIRAC);
            push(cadr(p1));
            list(2);
            return;
          }
          if (isnegativeterm(p1)) {
            push_symbol(DIRAC);
            push(p1);
            negate();
            list(2);
            return;
          }
          if (isnegativeterm(p1) || car(p1) === symbol(ADD) && isnegativeterm(cadr(p1))) {
            push(p1);
            negate();
            p1 = pop();
          }
          push_symbol(DIRAC);
          push(p1);
          return list(2);
        };
        divisors = function() {
          var h5, i5, n9, o12, ref2, subsetOfStack;
          i5 = 0;
          h5 = 0;
          n9 = 0;
          save();
          h5 = tos - 1;
          divisors_onstack();
          n9 = tos - h5;
          subsetOfStack = stack.slice(h5, h5 + n9);
          subsetOfStack.sort(cmp_expr);
          stack = stack.slice(0, h5).concat(subsetOfStack).concat(stack.slice(h5 + n9));
          p1 = alloc_tensor(n9);
          p1.tensor.ndim = 1;
          p1.tensor.dim[0] = n9;
          for (i5 = o12 = 0, ref2 = n9; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            p1.tensor.elem[i5] = stack[h5 + i5];
          }
          moveTos(h5);
          push(p1);
          return restore();
        };
        divisors_onstack = function() {
          var h5, i5, k2, n9, o12, ref2;
          h5 = 0;
          i5 = 0;
          k2 = 0;
          n9 = 0;
          save();
          p1 = pop();
          h5 = tos;
          if (isNumericAtom(p1)) {
            push(p1);
            factor_small_number();
          } else if (car(p1) === symbol(ADD)) {
            push(p1);
            __factor_add();
          } else if (car(p1) === symbol(MULTIPLY)) {
            p1 = cdr(p1);
            if (isNumericAtom(car(p1))) {
              push(car(p1));
              factor_small_number();
              p1 = cdr(p1);
            }
            while (iscons(p1)) {
              p2 = car(p1);
              if (car(p2) === symbol(POWER)) {
                push(cadr(p2));
                push(caddr(p2));
              } else {
                push(p2);
                push(one);
              }
              p1 = cdr(p1);
            }
          } else if (car(p1) === symbol(POWER)) {
            push(cadr(p1));
            push(caddr(p1));
          } else {
            push(p1);
            push(one);
          }
          k2 = tos;
          push(one);
          gen(h5, k2);
          n9 = tos - k2;
          for (i5 = o12 = 0, ref2 = n9; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            stack[h5 + i5] = stack[k2 + i5];
          }
          moveTos(h5 + n9);
          return restore();
        };
        gen = function(h5, k2) {
          var expo, i5, o12, ref2;
          expo = 0;
          i5 = 0;
          save();
          p1 = pop();
          if (h5 === k2) {
            push(p1);
            restore();
            return;
          }
          p2 = stack[h5 + 0];
          p3 = stack[h5 + 1];
          push(p3);
          expo = pop_integer();
          if (!isNaN(expo)) {
            for (i5 = o12 = 0, ref2 = Math.abs(expo); 0 <= ref2 ? o12 <= ref2 : o12 >= ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
              push(p1);
              push(p2);
              push_integer(sign(expo) * i5);
              power();
              multiply();
              gen(h5 + 2, k2);
            }
          }
          return restore();
        };
        __factor_add = function() {
          save();
          p1 = pop();
          p3 = cdr(p1);
          push(car(p3));
          p3 = cdr(p3);
          while (iscons(p3)) {
            push(car(p3));
            gcd();
            p3 = cdr(p3);
          }
          p2 = pop();
          if (isplusone(p2)) {
            push(p1);
            push(one);
            restore();
            return;
          }
          if (isNumericAtom(p2)) {
            push(p2);
            factor_small_number();
          } else if (car(p2) === symbol(MULTIPLY)) {
            p3 = cdr(p2);
            if (isNumericAtom(car(p3))) {
              push(car(p3));
              factor_small_number();
            } else {
              push(car(p3));
              push(one);
            }
            p3 = cdr(p3);
            while (iscons(p3)) {
              push(car(p3));
              push(one);
              p3 = cdr(p3);
            }
          } else {
            push(p2);
            push(one);
          }
          push(p2);
          inverse();
          p2 = pop();
          push(zero);
          p3 = cdr(p1);
          while (iscons(p3)) {
            push(p2);
            push(car(p3));
            multiply();
            add();
            p3 = cdr(p3);
          }
          push(one);
          return restore();
        };
        dpow = function() {
          var a4, b2, base, expo, result, theta;
          a4 = 0;
          b2 = 0;
          base = 0;
          expo = 0;
          result = 0;
          theta = 0;
          expo = pop_double();
          base = pop_double();
          if (base === 0 && expo < 0) {
            stop("divide by zero");
          }
          if (base >= 0 || expo % 1 === 0) {
            result = Math.pow(base, expo);
            push_double(result);
            return;
          }
          result = Math.pow(Math.abs(base), expo);
          theta = Math.PI * expo;
          if (expo % 0.5 === 0) {
            a4 = 0;
            b2 = Math.sin(theta);
          } else {
            a4 = Math.cos(theta);
            b2 = Math.sin(theta);
          }
          push_double(a4 * result);
          push_double(b2 * result);
          push(imaginaryunit);
          multiply();
          return add();
        };
        EIG_N = 0;
        EIG_yydd = [];
        EIG_yyqq = [];
        Eval_eigen = function() {
          if (EIG_check_arg() === 0) {
            stop("eigen: argument is not a square matrix");
          }
          eigen(EIGEN);
          p1 = usr_symbol("D");
          set_binding(p1, p2);
          p1 = usr_symbol("Q");
          set_binding(p1, p3);
          return push(symbol(NIL));
        };
        Eval_eigenval = function() {
          if (EIG_check_arg() === 0) {
            push_symbol(EIGENVAL);
            push(p1);
            list(2);
            return;
          }
          eigen(EIGENVAL);
          return push(p2);
        };
        Eval_eigenvec = function() {
          if (EIG_check_arg() === 0) {
            push_symbol(EIGENVEC);
            push(p1);
            list(2);
            return;
          }
          eigen(EIGENVEC);
          return push(p3);
        };
        EIG_check_arg = function() {
          var i5, i12, j2, j12, l1, o12, ref2, ref12, ref22, ref3, ref4;
          i5 = 0;
          j2 = 0;
          push(cadr(p1));
          Eval();
          yyfloat();
          Eval();
          p1 = pop();
          if (!istensor(p1)) {
            return 0;
          }
          if (p1.tensor.ndim !== 2 || p1.tensor.dim[0] !== p1.tensor.dim[1]) {
            stop("eigen: argument is not a square matrix");
          }
          EIG_N = p1.tensor.dim[0];
          for (i5 = o12 = 0, ref2 = EIG_N; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            for (j2 = i12 = 0, ref12 = EIG_N; 0 <= ref12 ? i12 < ref12 : i12 > ref12; j2 = 0 <= ref12 ? ++i12 : --i12) {
              if (!isdouble(p1.tensor.elem[EIG_N * i5 + j2])) {
                stop("eigen: matrix is not numerical");
              }
            }
          }
          for (i5 = j12 = 0, ref22 = EIG_N - 1; 0 <= ref22 ? j12 < ref22 : j12 > ref22; i5 = 0 <= ref22 ? ++j12 : --j12) {
            for (j2 = l1 = ref3 = i5 + 1, ref4 = EIG_N; ref3 <= ref4 ? l1 < ref4 : l1 > ref4; j2 = ref3 <= ref4 ? ++l1 : --l1) {
              if (Math.abs(p1.tensor.elem[EIG_N * i5 + j2].d - p1.tensor.elem[EIG_N * j2 + i5].d) > 1e-10) {
                stop("eigen: matrix is not symmetrical");
              }
            }
          }
          return 1;
        };
        eigen = function(op) {
          var i5, i12, j2, j12, l1, m1, n1, o12, o1, q1, r1, ref2, ref12, ref10, ref22, ref3, ref4, ref5, ref6, ref7, ref8, ref9, results, s1;
          i5 = 0;
          j2 = 0;
          for (i5 = o12 = 0, ref2 = EIG_N * EIG_N; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            EIG_yydd[i5] = 0;
          }
          for (i5 = i12 = 0, ref12 = EIG_N * EIG_N; 0 <= ref12 ? i12 < ref12 : i12 > ref12; i5 = 0 <= ref12 ? ++i12 : --i12) {
            EIG_yyqq[i5] = 0;
          }
          for (i5 = j12 = 0, ref22 = EIG_N; 0 <= ref22 ? j12 < ref22 : j12 > ref22; i5 = 0 <= ref22 ? ++j12 : --j12) {
            EIG_yydd[EIG_N * i5 + i5] = p1.tensor.elem[EIG_N * i5 + i5].d;
            for (j2 = l1 = ref3 = i5 + 1, ref4 = EIG_N; ref3 <= ref4 ? l1 < ref4 : l1 > ref4; j2 = ref3 <= ref4 ? ++l1 : --l1) {
              EIG_yydd[EIG_N * i5 + j2] = p1.tensor.elem[EIG_N * i5 + j2].d;
              EIG_yydd[EIG_N * j2 + i5] = p1.tensor.elem[EIG_N * i5 + j2].d;
            }
          }
          for (i5 = m1 = 0, ref5 = EIG_N; 0 <= ref5 ? m1 < ref5 : m1 > ref5; i5 = 0 <= ref5 ? ++m1 : --m1) {
            EIG_yyqq[EIG_N * i5 + i5] = 1;
            for (j2 = n1 = ref6 = i5 + 1, ref7 = EIG_N; ref6 <= ref7 ? n1 < ref7 : n1 > ref7; j2 = ref6 <= ref7 ? ++n1 : --n1) {
              EIG_yyqq[EIG_N * i5 + j2] = 0;
              EIG_yyqq[EIG_N * j2 + i5] = 0;
            }
          }
          for (i5 = o1 = 0; o1 < 100; i5 = ++o1) {
            if (step() === 0) {
              break;
            }
          }
          if (i5 === 100) {
            printstr("\nnote: eigen did not converge\n");
          }
          if (op === EIGEN || op === EIGENVAL) {
            push(p1);
            copy_tensor();
            p2 = pop();
            for (i5 = q1 = 0, ref8 = EIG_N; 0 <= ref8 ? q1 < ref8 : q1 > ref8; i5 = 0 <= ref8 ? ++q1 : --q1) {
              for (j2 = r1 = 0, ref9 = EIG_N; 0 <= ref9 ? r1 < ref9 : r1 > ref9; j2 = 0 <= ref9 ? ++r1 : --r1) {
                push_double(EIG_yydd[EIG_N * i5 + j2]);
                p2.tensor.elem[EIG_N * i5 + j2] = pop();
              }
            }
          }
          if (op === EIGEN || op === EIGENVEC) {
            push(p1);
            copy_tensor();
            p3 = pop();
            results = [];
            for (i5 = s1 = 0, ref10 = EIG_N; 0 <= ref10 ? s1 < ref10 : s1 > ref10; i5 = 0 <= ref10 ? ++s1 : --s1) {
              results.push(function() {
                var ref11, results1, t1;
                results1 = [];
                for (j2 = t1 = 0, ref11 = EIG_N; 0 <= ref11 ? t1 < ref11 : t1 > ref11; j2 = 0 <= ref11 ? ++t1 : --t1) {
                  push_double(EIG_yyqq[EIG_N * i5 + j2]);
                  results1.push(p3.tensor.elem[EIG_N * i5 + j2] = pop());
                }
                return results1;
              }());
            }
            return results;
          }
        };
        step = function() {
          var count2, i5, i12, j2, o12, ref2, ref12, ref22;
          i5 = 0;
          j2 = 0;
          count2 = 0;
          for (i5 = o12 = 0, ref2 = EIG_N - 1; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            for (j2 = i12 = ref12 = i5 + 1, ref22 = EIG_N; ref12 <= ref22 ? i12 < ref22 : i12 > ref22; j2 = ref12 <= ref22 ? ++i12 : --i12) {
              if (EIG_yydd[EIG_N * i5 + j2] !== 0) {
                step2(i5, j2);
                count2++;
              }
            }
          }
          return count2;
        };
        step2 = function(p11, q) {
          var c6, cc, i12, j12, k2, o12, ref2, ref12, ref22, s7, ss, t5, theta;
          k2 = 0;
          t5 = 0;
          theta = 0;
          c6 = 0;
          cc = 0;
          s7 = 0;
          ss = 0;
          theta = 0.5 * (EIG_yydd[EIG_N * p11 + p11] - EIG_yydd[EIG_N * q + q]) / EIG_yydd[EIG_N * p11 + q];
          t5 = 1 / (Math.abs(theta) + Math.sqrt(theta * theta + 1));
          if (theta < 0) {
            t5 = -t5;
          }
          c6 = 1 / Math.sqrt(t5 * t5 + 1);
          s7 = t5 * c6;
          for (k2 = o12 = 0, ref2 = EIG_N; 0 <= ref2 ? o12 < ref2 : o12 > ref2; k2 = 0 <= ref2 ? ++o12 : --o12) {
            cc = EIG_yydd[EIG_N * p11 + k2];
            ss = EIG_yydd[EIG_N * q + k2];
            EIG_yydd[EIG_N * p11 + k2] = c6 * cc + s7 * ss;
            EIG_yydd[EIG_N * q + k2] = c6 * ss - s7 * cc;
          }
          for (k2 = i12 = 0, ref12 = EIG_N; 0 <= ref12 ? i12 < ref12 : i12 > ref12; k2 = 0 <= ref12 ? ++i12 : --i12) {
            cc = EIG_yydd[EIG_N * k2 + p11];
            ss = EIG_yydd[EIG_N * k2 + q];
            EIG_yydd[EIG_N * k2 + p11] = c6 * cc + s7 * ss;
            EIG_yydd[EIG_N * k2 + q] = c6 * ss - s7 * cc;
          }
          for (k2 = j12 = 0, ref22 = EIG_N; 0 <= ref22 ? j12 < ref22 : j12 > ref22; k2 = 0 <= ref22 ? ++j12 : --j12) {
            cc = EIG_yyqq[EIG_N * p11 + k2];
            ss = EIG_yyqq[EIG_N * q + k2];
            EIG_yyqq[EIG_N * p11 + k2] = c6 * cc + s7 * ss;
            EIG_yyqq[EIG_N * q + k2] = c6 * ss - s7 * cc;
          }
          EIG_yydd[EIG_N * p11 + q] = 0;
          return EIG_yydd[EIG_N * q + p11] = 0;
        };
        Eval_erf = function() {
          push(cadr(p1));
          Eval();
          return yerf();
        };
        yerf = function() {
          save();
          yyerf();
          return restore();
        };
        yyerf = function() {
          var d3;
          d3 = 0;
          p1 = pop();
          if (isdouble(p1)) {
            d3 = 1 - erfc(p1.d);
            push_double(d3);
            return;
          }
          if (isZeroAtomOrTensor(p1)) {
            push(zero);
            return;
          }
          if (isnegativeterm(p1)) {
            push_symbol(ERF);
            push(p1);
            negate();
            list(2);
            negate();
            return;
          }
          push_symbol(ERF);
          push(p1);
          list(2);
        };
        Eval_erfc = function() {
          push(cadr(p1));
          Eval();
          return yerfc();
        };
        yerfc = function() {
          save();
          yyerfc();
          return restore();
        };
        yyerfc = function() {
          var d3;
          d3 = 0;
          p1 = pop();
          if (isdouble(p1)) {
            d3 = erfc(p1.d);
            push_double(d3);
            return;
          }
          if (isZeroAtomOrTensor(p1)) {
            push(one);
            return;
          }
          push_symbol(ERFC);
          push(p1);
          list(2);
        };
        erfc = function(x2) {
          var ans, t5, z2;
          if (x2 === 0) {
            return 1;
          }
          t5 = 0;
          z2 = 0;
          ans = 0;
          z2 = Math.abs(x2);
          t5 = 1 / (1 + 0.5 * z2);
          ans = t5 * Math.exp(-z2 * z2 - 1.26551223 + t5 * (1.00002368 + t5 * (0.37409196 + t5 * (0.09678418 + t5 * (-0.18628806 + t5 * (0.27886807 + t5 * (-1.13520398 + t5 * (1.48851587 + t5 * (-0.82215223 + t5 * 0.17087277)))))))));
          if (x2 >= 0) {
            return ans;
          } else {
            return 2 - ans;
          }
        };
        Eval = function() {
          var willEvaluateAsFloats;
          check_esc_flag();
          save();
          p1 = pop();
          if (p1 == null) {
            debugger;
          }
          if (!evaluatingAsFloats && isfloating(p1)) {
            willEvaluateAsFloats = true;
            evaluatingAsFloats++;
          }
          switch (p1.k) {
            case CONS:
              Eval_cons();
              break;
            case NUM:
              if (evaluatingAsFloats) {
                push_double(convert_rational_to_double(p1));
              } else {
                push(p1);
              }
              break;
            case DOUBLE:
            case STR:
              push(p1);
              break;
            case TENSOR:
              Eval_tensor();
              break;
            case SYM:
              Eval_sym();
              break;
            default:
              stop("atom?");
          }
          if (willEvaluateAsFloats) {
            evaluatingAsFloats--;
          }
          return restore();
        };
        Eval_sym = function() {
          var cycleString, i5, o12, positionIfSymbolAlreadyBeingEvaluated, ref2, ref12;
          if (iskeyword(p1)) {
            push(p1);
            push(symbol(LAST));
            list(2);
            Eval();
            return;
          } else if (p1 === symbol(PI) && evaluatingAsFloats) {
            push_double(Math.PI);
            return;
          }
          p2 = get_binding(p1);
          if (DEBUG) {
            console.log("looked up: " + p1 + " which contains: " + p2);
          }
          push(p2);
          if (p1 !== p2) {
            positionIfSymbolAlreadyBeingEvaluated = chainOfUserSymbolsNotFunctionsBeingEvaluated.indexOf(p1);
            if (positionIfSymbolAlreadyBeingEvaluated !== -1) {
              cycleString = "";
              for (i5 = o12 = ref2 = positionIfSymbolAlreadyBeingEvaluated, ref12 = chainOfUserSymbolsNotFunctionsBeingEvaluated.length; ref2 <= ref12 ? o12 < ref12 : o12 > ref12; i5 = ref2 <= ref12 ? ++o12 : --o12) {
                cycleString += chainOfUserSymbolsNotFunctionsBeingEvaluated[i5].printname + " -> ";
              }
              cycleString += p1.printname;
              stop("recursive evaluation of symbols: " + cycleString);
              return;
            }
            chainOfUserSymbolsNotFunctionsBeingEvaluated.push(p1);
            Eval();
            return chainOfUserSymbolsNotFunctionsBeingEvaluated.pop();
          }
        };
        Eval_cons = function() {
          var cons_head;
          cons_head = car(p1);
          if (car(cons_head) === symbol(EVAL)) {
            Eval_user_function();
            return;
          }
          if (!issymbol(cons_head)) {
            stop("cons?");
          }
          switch (symnum(cons_head)) {
            case ABS:
              return Eval_abs();
            case ADD:
              return Eval_add();
            case ADJ:
              return Eval_adj();
            case AND:
              return Eval_and();
            case ARCCOS:
              return Eval_arccos();
            case ARCCOSH:
              return Eval_arccosh();
            case ARCSIN:
              return Eval_arcsin();
            case ARCSINH:
              return Eval_arcsinh();
            case ARCTAN:
              return Eval_arctan();
            case ARCTANH:
              return Eval_arctanh();
            case ARG:
              return Eval_arg();
            case ATOMIZE:
              return Eval_atomize();
            case BESSELJ:
              return Eval_besselj();
            case BESSELY:
              return Eval_bessely();
            case BINDING:
              return Eval_binding();
            case BINOMIAL:
              return Eval_binomial();
            case CEILING:
              return Eval_ceiling();
            case CHECK:
              return Eval_check();
            case CHOOSE:
              return Eval_choose();
            case CIRCEXP:
              return Eval_circexp();
            case CLEAR:
              return Eval_clear();
            case CLEARALL:
              return Eval_clearall();
            case CLEARPATTERNS:
              return Eval_clearpatterns();
            case CLOCK:
              return Eval_clock();
            case COEFF:
              return Eval_coeff();
            case COFACTOR:
              return Eval_cofactor();
            case CONDENSE:
              return Eval_condense();
            case CONJ:
              return Eval_conj();
            case CONTRACT:
              return Eval_contract();
            case COS:
              return Eval_cos();
            case COSH:
              return Eval_cosh();
            case DECOMP:
              return Eval_decomp();
            case DEGREE:
              return Eval_degree();
            case DEFINT:
              return Eval_defint();
            case DENOMINATOR:
              return Eval_denominator();
            case DERIVATIVE:
              return Eval_derivative();
            case DET:
              return Eval_det();
            case DIM:
              return Eval_dim();
            case DIRAC:
              return Eval_dirac();
            case DIVISORS:
              return Eval_divisors();
            case DO:
              return Eval_do();
            case DOT:
              return Eval_inner();
            case DRAW:
              return Eval_draw();
            case DSOLVE:
              return Eval_dsolve();
            case EIGEN:
              return Eval_eigen();
            case EIGENVAL:
              return Eval_eigenval();
            case EIGENVEC:
              return Eval_eigenvec();
            case ERF:
              return Eval_erf();
            case ERFC:
              return Eval_erfc();
            case EVAL:
              return Eval_Eval();
            case EXP:
              return Eval_exp();
            case EXPAND:
              return Eval_expand();
            case EXPCOS:
              return Eval_expcos();
            case EXPSIN:
              return Eval_expsin();
            case FACTOR:
              return Eval_factor();
            case FACTORIAL:
              return Eval_factorial();
            case FACTORPOLY:
              return Eval_factorpoly();
            case FILTER:
              return Eval_filter();
            case FLOATF:
              return Eval_float();
            case APPROXRATIO:
              return Eval_approxratio();
            case FLOOR:
              return Eval_floor();
            case FOR:
              return Eval_for();
            // this is invoked only when we
            // evaluate a function that is NOT being called
            // e.g. when f is a function as we do
            //  g = f
            case FUNCTION:
              return Eval_function_reference();
            case GAMMA:
              return Eval_gamma();
            case GCD:
              return Eval_gcd();
            case HERMITE:
              return Eval_hermite();
            case HILBERT:
              return Eval_hilbert();
            case IMAG:
              return Eval_imag();
            case INDEX:
              return Eval_index();
            case INNER:
              return Eval_inner();
            case INTEGRAL:
              return Eval_integral();
            case INV:
              return Eval_inv();
            case INVG:
              return Eval_invg();
            case ISINTEGER:
              return Eval_isinteger();
            case ISPRIME:
              return Eval_isprime();
            case LAGUERRE:
              return Eval_laguerre();
            //  when LAPLACE then Eval_laplace()
            case LCM:
              return Eval_lcm();
            case LEADING:
              return Eval_leading();
            case LEGENDRE:
              return Eval_legendre();
            case LOG:
              return Eval_log();
            case LOOKUP:
              return Eval_lookup();
            case MOD:
              return Eval_mod();
            case MULTIPLY:
              return Eval_multiply();
            case NOT:
              return Eval_not();
            case NROOTS:
              return Eval_nroots();
            case NUMBER:
              return Eval_number();
            case NUMERATOR:
              return Eval_numerator();
            case OPERATOR:
              return Eval_operator();
            case OR:
              return Eval_or();
            case OUTER:
              return Eval_outer();
            case PATTERN:
              return Eval_pattern();
            case PATTERNSINFO:
              return Eval_patternsinfo();
            case POLAR:
              return Eval_polar();
            case POWER:
              return Eval_power();
            case PRIME:
              return Eval_prime();
            case PRINT:
              return Eval_print();
            case PRINT2DASCII:
              return Eval_print2dascii();
            case PRINTFULL:
              return Eval_printcomputer();
            case PRINTLATEX:
              return Eval_printlatex();
            case PRINTLIST:
              return Eval_printlist();
            case PRINTPLAIN:
              return Eval_printhuman();
            case PRODUCT:
              return Eval_product();
            case QUOTE:
              return Eval_quote();
            case QUOTIENT:
              return Eval_quotient();
            case RANK:
              return Eval_rank();
            case RATIONALIZE:
              return Eval_rationalize();
            case REAL:
              return Eval_real();
            case ROUND:
              return Eval_round();
            case YYRECT:
              return Eval_rect();
            case ROOTS:
              return Eval_roots();
            case SETQ:
              return Eval_setq();
            case SGN:
              return Eval_sgn();
            case SILENTPATTERN:
              return Eval_silentpattern();
            case SIMPLIFY:
              return Eval_simplify();
            case SIN:
              return Eval_sin();
            case SINH:
              return Eval_sinh();
            case SHAPE:
              return Eval_shape();
            case SQRT:
              return Eval_sqrt();
            case STOP:
              return Eval_stop();
            case SUBST:
              return Eval_subst();
            case SUM:
              return Eval_sum();
            case SYMBOLSINFO:
              return Eval_symbolsinfo();
            case TAN:
              return Eval_tan();
            case TANH:
              return Eval_tanh();
            case TAYLOR:
              return Eval_taylor();
            case TEST:
              return Eval_test();
            case TESTEQ:
              return Eval_testeq();
            case TESTGE:
              return Eval_testge();
            case TESTGT:
              return Eval_testgt();
            case TESTLE:
              return Eval_testle();
            case TESTLT:
              return Eval_testlt();
            case TRANSPOSE:
              return Eval_transpose();
            case UNIT:
              return Eval_unit();
            case ZERO:
              return Eval_zero();
            default:
              return Eval_user_function();
          }
        };
        Eval_binding = function() {
          return push(get_binding(cadr(p1)));
        };
        Eval_check = function() {
          var checkResult;
          checkResult = isZeroLikeOrNonZeroLikeOrUndetermined(cadr(p1));
          if (checkResult == null) {
            return push(p1);
          } else {
            return push_integer(checkResult);
          }
        };
        Eval_det = function() {
          push(cadr(p1));
          Eval();
          return det();
        };
        Eval_dim = function() {
          var n9;
          push(cadr(p1));
          Eval();
          p2 = pop();
          if (iscons(cddr(p1))) {
            push(caddr(p1));
            Eval();
            n9 = pop_integer();
          } else {
            n9 = 1;
          }
          if (!istensor(p2)) {
            return push_integer(1);
          } else if (n9 < 1 || n9 > p2.tensor.ndim) {
            return push(p1);
          } else {
            return push_integer(p2.tensor.dim[n9 - 1]);
          }
        };
        Eval_divisors = function() {
          push(cadr(p1));
          Eval();
          return divisors();
        };
        Eval_do = function() {
          var results;
          push(car(p1));
          p1 = cdr(p1);
          results = [];
          while (iscons(p1)) {
            pop();
            push(car(p1));
            Eval();
            results.push(p1 = cdr(p1));
          }
          return results;
        };
        Eval_dsolve = function() {
          push(cadr(p1));
          Eval();
          push(caddr(p1));
          Eval();
          push(cadddr(p1));
          Eval();
          return dsolve();
        };
        Eval_Eval = function() {
          push(cadr(p1));
          Eval();
          p1 = cddr(p1);
          while (iscons(p1)) {
            push(car(p1));
            Eval();
            push(cadr(p1));
            Eval();
            subst();
            p1 = cddr(p1);
          }
          return Eval();
        };
        Eval_exp = function() {
          push(cadr(p1));
          Eval();
          return exponential();
        };
        Eval_factorial = function() {
          push(cadr(p1));
          Eval();
          return factorial();
        };
        Eval_factorpoly = function() {
          var results;
          p1 = cdr(p1);
          push(car(p1));
          Eval();
          p1 = cdr(p1);
          push(car(p1));
          Eval();
          factorpoly();
          p1 = cdr(p1);
          results = [];
          while (iscons(p1)) {
            push(car(p1));
            Eval();
            factorpoly();
            results.push(p1 = cdr(p1));
          }
          return results;
        };
        Eval_hermite = function() {
          push(cadr(p1));
          Eval();
          push(caddr(p1));
          Eval();
          return hermite();
        };
        Eval_hilbert = function() {
          push(cadr(p1));
          Eval();
          return hilbert();
        };
        Eval_index = function() {
          var h5, orig, theTensor;
          h5 = tos;
          orig = p1;
          p1 = cdr(p1);
          push(car(p1));
          Eval();
          theTensor = stack[tos - 1];
          if (isNumericAtom(theTensor)) {
            stop("trying to access a scalar as a tensor");
          }
          if (!istensor(theTensor)) {
            moveTos(h5);
            push(orig);
            return;
          }
          p1 = cdr(p1);
          while (iscons(p1)) {
            push(car(p1));
            Eval();
            if (!isintegerorintegerfloat(stack[tos - 1])) {
              moveTos(h5);
              push(orig);
              return;
            }
            p1 = cdr(p1);
          }
          return index_function(tos - h5);
        };
        Eval_inv = function() {
          push(cadr(p1));
          Eval();
          return inv();
        };
        Eval_invg = function() {
          push(cadr(p1));
          Eval();
          return invg();
        };
        Eval_isinteger = function() {
          var n9;
          push(cadr(p1));
          Eval();
          p1 = pop();
          if (isrational(p1)) {
            if (isinteger(p1)) {
              push(one);
            } else {
              push(zero);
            }
            return;
          }
          if (isdouble(p1)) {
            n9 = Math.floor(p1.d);
            if (n9 === p1.d) {
              push(one);
            } else {
              push(zero);
            }
            return;
          }
          push_symbol(ISINTEGER);
          push(p1);
          return list(2);
        };
        Eval_number = function() {
          push(cadr(p1));
          Eval();
          p1 = pop();
          if (p1.k === NUM || p1.k === DOUBLE) {
            return push_integer(1);
          } else {
            return push_integer(0);
          }
        };
        Eval_operator = function() {
          var h5;
          h5 = tos;
          push_symbol(OPERATOR);
          p1 = cdr(p1);
          while (iscons(p1)) {
            push(car(p1));
            Eval();
            p1 = cdr(p1);
          }
          return list(tos - h5);
        };
        Eval_quote = function() {
          return push(cadr(p1));
        };
        Eval_rank = function() {
          push(cadr(p1));
          Eval();
          p1 = pop();
          if (istensor(p1)) {
            return push_integer(p1.tensor.ndim);
          } else {
            return push(zero);
          }
        };
        Eval_setq = function() {
          if (caadr(p1) === symbol(INDEX)) {
            setq_indexed();
            return;
          }
          if (iscons(cadr(p1))) {
            define_user_function();
            return;
          }
          if (!issymbol(cadr(p1))) {
            stop("symbol assignment: error in symbol");
          }
          push(caddr(p1));
          Eval();
          p2 = pop();
          set_binding(cadr(p1), p2);
          return push(symbol(NIL));
        };
        setq_indexed = function() {
          var h5;
          p4 = cadadr(p1);
          if (!issymbol(p4)) {
            stop("indexed assignment: expected a symbol name");
          }
          h5 = tos;
          push(caddr(p1));
          Eval();
          p2 = cdadr(p1);
          while (iscons(p2)) {
            push(car(p2));
            Eval();
            p2 = cdr(p2);
          }
          set_component(tos - h5);
          p3 = pop();
          set_binding(p4, p3);
          return push(symbol(NIL));
        };
        Eval_sqrt = function() {
          push(cadr(p1));
          Eval();
          push_rational(1, 2);
          return power();
        };
        Eval_stop = function() {
          return stop("user stop");
        };
        Eval_subst = function() {
          push(cadddr(p1));
          Eval();
          push(caddr(p1));
          Eval();
          push(cadr(p1));
          Eval();
          subst();
          return Eval();
        };
        Eval_unit = function() {
          var i5, n9, o12, ref2;
          i5 = 0;
          n9 = 0;
          push(cadr(p1));
          Eval();
          n9 = pop_integer();
          if (isNaN(n9)) {
            push(p1);
            return;
          }
          if (n9 < 1) {
            push(p1);
            return;
          }
          p1 = alloc_tensor(n9 * n9);
          p1.tensor.ndim = 2;
          p1.tensor.dim[0] = n9;
          p1.tensor.dim[1] = n9;
          for (i5 = o12 = 0, ref2 = n9; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            p1.tensor.elem[n9 * i5 + i5] = one;
          }
          check_tensor_dimensions(p1);
          return push(p1);
        };
        Eval_noexpand = function() {
          var prev_expanding;
          prev_expanding = expanding;
          expanding = 0;
          Eval();
          return expanding = prev_expanding;
        };
        Eval_predicate = function() {
          save();
          p1 = top();
          if (car(p1) === symbol(SETQ)) {
            pop();
            push_symbol(TESTEQ);
            push(cadr(p1));
            push(caddr(p1));
            list(3);
          }
          Eval();
          return restore();
        };
        Eval_expand = function() {
          push(cadr(p1));
          Eval();
          push(caddr(p1));
          Eval();
          p2 = pop();
          if (p2 === symbol(NIL)) {
            guess();
          } else {
            push(p2);
          }
          return expand();
        };
        expand = function() {
          var prev_expanding;
          save();
          p9 = pop();
          p5 = pop();
          if (istensor(p5)) {
            expand_tensor();
            restore();
            return;
          }
          if (car(p5) === symbol(ADD)) {
            push_integer(0);
            p1 = cdr(p5);
            while (iscons(p1)) {
              push(car(p1));
              push(p9);
              expand();
              add();
              p1 = cdr(p1);
            }
            restore();
            return;
          }
          push(p5);
          numerator();
          p3 = pop();
          push(p5);
          denominator();
          p2 = pop();
          remove_negative_exponents();
          push(p3);
          push(p2);
          push(p9);
          if (isone(p3) || isone(p2)) {
            if (!ispolyexpandedform(p2, p9) || isone(p2)) {
              pop();
              pop();
              pop();
              push(p5);
              restore();
              return;
            }
          }
          divpoly();
          p7 = pop();
          push(p3);
          push(p2);
          push(p7);
          multiply();
          subtract();
          p3 = pop();
          if (isZeroAtomOrTensor(p3)) {
            push(p7);
            restore();
            return;
          }
          push(p2);
          push(p9);
          factorpoly();
          p2 = pop();
          expand_get_C();
          expand_get_B();
          expand_get_A();
          if (istensor(p4)) {
            push(p4);
            prev_expanding = expanding;
            expanding = 1;
            inv();
            expanding = prev_expanding;
            push(p3);
            inner();
            push(p2);
            inner();
          } else {
            push(p3);
            push(p4);
            prev_expanding = expanding;
            expanding = 1;
            divide();
            expanding = prev_expanding;
            push(p2);
            multiply();
          }
          push(p7);
          add();
          return restore();
        };
        expand_tensor = function() {
          var i5, o12, ref2;
          i5 = 0;
          push(p5);
          copy_tensor();
          p5 = pop();
          for (i5 = o12 = 0, ref2 = p5.tensor.nelem; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            push(p5.tensor.elem[i5]);
            push(p9);
            expand();
            p5.tensor.elem[i5] = pop();
          }
          return push(p5);
        };
        remove_negative_exponents = function() {
          var h5, i5, j2, k2, n9, o12, ref2;
          h5 = 0;
          i5 = 0;
          j2 = 0;
          k2 = 0;
          n9 = 0;
          h5 = tos;
          factors(p2);
          factors(p3);
          n9 = tos - h5;
          j2 = 0;
          for (i5 = o12 = 0, ref2 = n9; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            p1 = stack[h5 + i5];
            if (car(p1) !== symbol(POWER)) {
              continue;
            }
            if (cadr(p1) !== p9) {
              continue;
            }
            push(caddr(p1));
            k2 = pop_integer();
            if (isNaN(k2)) {
              continue;
            }
            if (k2 < j2) {
              j2 = k2;
            }
          }
          moveTos(h5);
          if (j2 === 0) {
            return;
          }
          push(p2);
          push(p9);
          push_integer(-j2);
          power();
          multiply();
          p2 = pop();
          push(p3);
          push(p9);
          push_integer(-j2);
          power();
          multiply();
          return p3 = pop();
        };
        expand_get_C = function() {
          var a4, h5, i5, i12, j2, n9, o12, prev_expanding, ref2, ref12;
          h5 = 0;
          i5 = 0;
          j2 = 0;
          n9 = 0;
          h5 = tos;
          if (car(p2) === symbol(MULTIPLY)) {
            p1 = cdr(p2);
            while (iscons(p1)) {
              p5 = car(p1);
              expand_get_CF();
              p1 = cdr(p1);
            }
          } else {
            p5 = p2;
            expand_get_CF();
          }
          n9 = tos - h5;
          if (n9 === 1) {
            p4 = pop();
            return;
          }
          p4 = alloc_tensor(n9 * n9);
          p4.tensor.ndim = 2;
          p4.tensor.dim[0] = n9;
          p4.tensor.dim[1] = n9;
          a4 = h5;
          for (i5 = o12 = 0, ref2 = n9; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            for (j2 = i12 = 0, ref12 = n9; 0 <= ref12 ? i12 < ref12 : i12 > ref12; j2 = 0 <= ref12 ? ++i12 : --i12) {
              push(stack[a4 + j2]);
              push(p9);
              push_integer(i5);
              power();
              prev_expanding = expanding;
              expanding = 1;
              divide();
              expanding = prev_expanding;
              push(p9);
              filter();
              p4.tensor.elem[n9 * i5 + j2] = pop();
            }
          }
          return moveTos(tos - n9);
        };
        expand_get_CF = function() {
          var d3, i5, j2, n9, o12, prev_expanding, ref2, results;
          d3 = 0;
          i5 = 0;
          j2 = 0;
          n9 = 0;
          if (!Find(p5, p9)) {
            return;
          }
          prev_expanding = expanding;
          expanding = 1;
          trivial_divide();
          expanding = prev_expanding;
          if (car(p5) === symbol(POWER)) {
            push(caddr(p5));
            n9 = pop_integer();
            p6 = cadr(p5);
          } else {
            n9 = 1;
            p6 = p5;
          }
          push(p6);
          push(p9);
          degree();
          d3 = pop_integer();
          results = [];
          for (i5 = o12 = 0, ref2 = n9; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            results.push(function() {
              var i12, ref12, results1;
              results1 = [];
              for (j2 = i12 = 0, ref12 = d3; 0 <= ref12 ? i12 < ref12 : i12 > ref12; j2 = 0 <= ref12 ? ++i12 : --i12) {
                push(p8);
                push(p6);
                push_integer(i5);
                power();
                prev_expanding = expanding;
                expanding = 1;
                multiply();
                expanding = prev_expanding;
                push(p9);
                push_integer(j2);
                power();
                prev_expanding = expanding;
                expanding = 1;
                multiply();
                results1.push(expanding = prev_expanding);
              }
              return results1;
            }());
          }
          return results;
        };
        trivial_divide = function() {
          var h5;
          h5 = 0;
          if (car(p2) === symbol(MULTIPLY)) {
            h5 = tos;
            p0 = cdr(p2);
            while (iscons(p0)) {
              if (!equal(car(p0), p5)) {
                push(car(p0));
                Eval();
              }
              p0 = cdr(p0);
            }
            multiply_all(tos - h5);
          } else {
            push_integer(1);
          }
          return p8 = pop();
        };
        expand_get_B = function() {
          var i5, n9, o12, prev_expanding, ref2;
          i5 = 0;
          n9 = 0;
          if (!istensor(p4)) {
            return;
          }
          n9 = p4.tensor.dim[0];
          p8 = alloc_tensor(n9);
          p8.tensor.ndim = 1;
          p8.tensor.dim[0] = n9;
          for (i5 = o12 = 0, ref2 = n9; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            push(p3);
            push(p9);
            push_integer(i5);
            power();
            prev_expanding = expanding;
            expanding = 1;
            divide();
            expanding = prev_expanding;
            push(p9);
            filter();
            p8.tensor.elem[i5] = pop();
          }
          return p3 = p8;
        };
        expand_get_A = function() {
          var h5, i5, n9, o12, ref2;
          h5 = 0;
          i5 = 0;
          n9 = 0;
          if (!istensor(p4)) {
            push(p2);
            reciprocate();
            p2 = pop();
            return;
          }
          h5 = tos;
          if (car(p2) === symbol(MULTIPLY)) {
            p8 = cdr(p2);
            while (iscons(p8)) {
              p5 = car(p8);
              expand_get_AF();
              p8 = cdr(p8);
            }
          } else {
            p5 = p2;
            expand_get_AF();
          }
          n9 = tos - h5;
          p8 = alloc_tensor(n9);
          p8.tensor.ndim = 1;
          p8.tensor.dim[0] = n9;
          for (i5 = o12 = 0, ref2 = n9; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            p8.tensor.elem[i5] = stack[h5 + i5];
          }
          moveTos(h5);
          return p2 = p8;
        };
        expand_get_AF = function() {
          var d3, i5, j2, n9, o12, ref2, results;
          d3 = 0;
          i5 = 0;
          j2 = 0;
          n9 = 1;
          if (!Find(p5, p9)) {
            return;
          }
          if (car(p5) === symbol(POWER)) {
            push(caddr(p5));
            n9 = pop_integer();
            p5 = cadr(p5);
          }
          push(p5);
          push(p9);
          degree();
          d3 = pop_integer();
          results = [];
          for (i5 = o12 = ref2 = n9; ref2 <= 0 ? o12 < 0 : o12 > 0; i5 = ref2 <= 0 ? ++o12 : --o12) {
            results.push(function() {
              var i12, ref12, results1;
              results1 = [];
              for (j2 = i12 = 0, ref12 = d3; 0 <= ref12 ? i12 < ref12 : i12 > ref12; j2 = 0 <= ref12 ? ++i12 : --i12) {
                push(p5);
                push_integer(i5);
                power();
                reciprocate();
                push(p9);
                push_integer(j2);
                power();
                results1.push(multiply());
              }
              return results1;
            }());
          }
          return results;
        };
        Eval_expcos = function() {
          push(cadr(p1));
          Eval();
          return expcos();
        };
        expcos = function() {
          save();
          p1 = pop();
          push(imaginaryunit);
          push(p1);
          multiply();
          exponential();
          push_rational(1, 2);
          multiply();
          push(imaginaryunit);
          negate();
          push(p1);
          multiply();
          exponential();
          push_rational(1, 2);
          multiply();
          add();
          return restore();
        };
        Eval_expsin = function() {
          push(cadr(p1));
          Eval();
          return expsin();
        };
        expsin = function() {
          save();
          p1 = pop();
          push(imaginaryunit);
          push(p1);
          multiply();
          exponential();
          push(imaginaryunit);
          divide();
          push_rational(1, 2);
          multiply();
          push(imaginaryunit);
          negate();
          push(p1);
          multiply();
          exponential();
          push(imaginaryunit);
          divide();
          push_rational(1, 2);
          multiply();
          subtract();
          return restore();
        };
        Eval_factor = function() {
          var results;
          push(cadr(p1));
          Eval();
          push(caddr(p1));
          Eval();
          p2 = pop();
          if (p2 === symbol(NIL)) {
            guess();
          } else {
            push(p2);
          }
          factor();
          p1 = cdddr(p1);
          results = [];
          while (iscons(p1)) {
            push(car(p1));
            Eval();
            factor_again();
            results.push(p1 = cdr(p1));
          }
          return results;
        };
        factor_again = function() {
          var h5, n9;
          save();
          p2 = pop();
          p1 = pop();
          h5 = tos;
          if (car(p1) === symbol(MULTIPLY)) {
            p1 = cdr(p1);
            while (iscons(p1)) {
              push(car(p1));
              push(p2);
              factor_term();
              p1 = cdr(p1);
            }
          } else {
            push(p1);
            push(p2);
            factor_term();
          }
          n9 = tos - h5;
          if (n9 > 1) {
            multiply_all_noexpand(n9);
          }
          return restore();
        };
        factor_term = function() {
          save();
          factorpoly();
          p1 = pop();
          if (car(p1) === symbol(MULTIPLY)) {
            p1 = cdr(p1);
            while (iscons(p1)) {
              push(car(p1));
              p1 = cdr(p1);
            }
          } else {
            push(p1);
          }
          return restore();
        };
        factor = function() {
          save();
          p2 = pop();
          p1 = pop();
          if (isinteger(p1)) {
            push(p1);
            factor_number();
          } else {
            push(p1);
            push(p2);
            factorpoly();
          }
          return restore();
        };
        factor_small_number = function() {
          var d3, expo, i5, n9, o12, ref2;
          i5 = 0;
          save();
          n9 = pop_integer();
          if (isNaN(n9)) {
            stop("number too big to factor");
          }
          if (n9 < 0) {
            n9 = -n9;
          }
          for (i5 = o12 = 0, ref2 = MAXPRIMETAB; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            d3 = primetab[i5];
            if (d3 > n9 / d3) {
              break;
            }
            expo = 0;
            while (n9 % d3 === 0) {
              n9 /= d3;
              expo++;
            }
            if (expo) {
              push_integer(d3);
              push_integer(expo);
            }
          }
          if (n9 > 1) {
            push_integer(n9);
            push_integer(1);
          }
          return restore();
        };
        factorial = function() {
          var n9;
          n9 = 0;
          save();
          p1 = pop();
          push(p1);
          n9 = pop_integer();
          if (n9 < 0 || isNaN(n9)) {
            push_symbol(FACTORIAL);
            push(p1);
            list(2);
            restore();
            return;
          }
          bignum_factorial(n9);
          return restore();
        };
        simplifyfactorials = function() {
          var x2;
          x2 = 0;
          save();
          x2 = expanding;
          expanding = 0;
          p1 = pop();
          if (car(p1) === symbol(ADD)) {
            push(zero);
            p1 = cdr(p1);
            while (iscons(p1)) {
              push(car(p1));
              simplifyfactorials();
              add();
              p1 = cdr(p1);
            }
            expanding = x2;
            restore();
            return;
          }
          if (car(p1) === symbol(MULTIPLY)) {
            sfac_product();
            expanding = x2;
            restore();
            return;
          }
          push(p1);
          expanding = x2;
          return restore();
        };
        sfac_product = function() {
          var i5, i12, j2, j12, n9, o12, ref2, ref12, ref22, ref3, s7;
          i5 = 0;
          j2 = 0;
          n9 = 0;
          s7 = tos;
          p1 = cdr(p1);
          n9 = 0;
          while (iscons(p1)) {
            push(car(p1));
            p1 = cdr(p1);
            n9++;
          }
          for (i5 = o12 = 0, ref2 = n9 - 1; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            if (stack[s7 + i5] === symbol(NIL)) {
              continue;
            }
            for (j2 = i12 = ref12 = i5 + 1, ref22 = n9; ref12 <= ref22 ? i12 < ref22 : i12 > ref22; j2 = ref12 <= ref22 ? ++i12 : --i12) {
              if (stack[s7 + j2] === symbol(NIL)) {
                continue;
              }
              sfac_product_f(s7, i5, j2);
            }
          }
          push(one);
          for (i5 = j12 = 0, ref3 = n9; 0 <= ref3 ? j12 < ref3 : j12 > ref3; i5 = 0 <= ref3 ? ++j12 : --j12) {
            if (stack[s7 + i5] === symbol(NIL)) {
              continue;
            }
            push(stack[s7 + i5]);
            multiply();
          }
          p1 = pop();
          moveTos(tos - n9);
          return push(p1);
        };
        sfac_product_f = function(s7, a4, b2) {
          var i5, n9, o12, ref2;
          i5 = 0;
          n9 = 0;
          p1 = stack[s7 + a4];
          p2 = stack[s7 + b2];
          if (ispower(p1)) {
            p3 = caddr(p1);
            p1 = cadr(p1);
          } else {
            p3 = one;
          }
          if (ispower(p2)) {
            p4 = caddr(p2);
            p2 = cadr(p2);
          } else {
            p4 = one;
          }
          if (isfactorial(p1) && isfactorial(p2)) {
            push(p3);
            push(p4);
            add();
            yyexpand();
            n9 = pop_integer();
            if (n9 !== 0) {
              return;
            }
            push(cadr(p1));
            push(cadr(p2));
            subtract();
            yyexpand();
            n9 = pop_integer();
            if (n9 === 0 || isNaN(n9)) {
              return;
            }
            if (n9 < 0) {
              n9 = -n9;
              p5 = p1;
              p1 = p2;
              p2 = p5;
              p5 = p3;
              p3 = p4;
              p4 = p5;
            }
            push(one);
            for (i5 = o12 = 1, ref2 = n9; 1 <= ref2 ? o12 <= ref2 : o12 >= ref2; i5 = 1 <= ref2 ? ++o12 : --o12) {
              push(cadr(p2));
              push_integer(i5);
              add();
              push(p3);
              power();
              multiply();
            }
            stack[s7 + a4] = pop();
            return stack[s7 + b2] = symbol(NIL);
          }
        };
        factorpoly = function() {
          var polynomial, variable;
          if (DEBUG) {
            console.log("factorpoly: " + stack[tos - 1].toString() + " " + stack[tos - 2].toString());
          }
          save();
          variable = pop();
          polynomial = pop();
          if (!Find(polynomial, variable) || !ispolyexpandedform(polynomial, variable) || !issymbol(variable)) {
            push(polynomial);
          } else {
            yyfactorpoly(variable, polynomial);
          }
          return restore();
        };
        yyfactorpoly = function(variable, polynomial) {
          var A2, AxPlusB, B2, checkingTheDivision, dividend, factpoly_expo, firstParam, foundComplexRoot, foundRealRoot, h5, i5, i12, j12, l1, o12, partOfPolynomialFactoredSoFar, polycoeff, prev_expanding, previousFactorisation, ref2, ref12, ref22, ref3, remainingPoly, secondDegreePloly, secondParam, whichRootsAreWeFinding;
          if (DEBUG) {
            firstParam = variable;
            secondParam = polynomial;
            console.log("yyfactorpoly: " + firstParam + " " + secondParam);
          }
          save();
          h5 = tos;
          if (isfloating(polynomial)) {
            stop("floating point numbers in polynomial");
          }
          polycoeff = tos;
          factpoly_expo = coeff(variable, polynomial) - 1;
          if (DEBUG) {
            console.log("yyfactorpoly: " + firstParam + " " + secondParam + " factpoly_expo before rationalize_coefficients: " + factpoly_expo);
          }
          partOfPolynomialFactoredSoFar = rationalize_coefficients(h5);
          if (DEBUG) {
            console.log("yyfactorpoly: " + firstParam + " " + secondParam + " factpoly_expo  after rationalize_coefficients: " + factpoly_expo);
          }
          whichRootsAreWeFinding = "real";
          remainingPoly = null;
          while (factpoly_expo > 0) {
            if (DEBUG) {
              console.log("yyfactorpoly: " + firstParam + " " + secondParam + " factpoly_expo inside while loop: " + factpoly_expo);
            }
            if (isZeroAtomOrTensor(stack[polycoeff + 0])) {
              if (DEBUG) {
                console.log("yyfactorpoly: " + firstParam + " " + secondParam + " isZeroAtomOrTensor");
              }
              A2 = one;
              B2 = zero;
            } else {
              if (whichRootsAreWeFinding === "real") {
                [foundRealRoot, A2, B2] = get_factor_from_real_root(variable, factpoly_expo, polycoeff);
              } else if (whichRootsAreWeFinding === "complex") {
                [foundComplexRoot, A2] = get_factor_from_complex_root(remainingPoly, factpoly_expo, polycoeff);
              }
            }
            if (whichRootsAreWeFinding === "real") {
              if (foundRealRoot === 0) {
                whichRootsAreWeFinding = "complex";
                continue;
              } else {
                push(A2);
                push(variable);
                multiply();
                push(B2);
                add();
                AxPlusB = pop();
                if (DEBUG) {
                  console.log("yyfactorpoly: " + firstParam + " " + secondParam + " success\nFACTOR=" + AxPlusB);
                }
                push(partOfPolynomialFactoredSoFar);
                push(AxPlusB);
                multiply_noexpand();
                partOfPolynomialFactoredSoFar = pop();
                yydivpoly(factpoly_expo, polycoeff, A2, B2);
                while (factpoly_expo && isZeroAtomOrTensor(stack[polycoeff + factpoly_expo])) {
                  factpoly_expo--;
                }
                push(zero);
                for (i5 = o12 = 0, ref2 = factpoly_expo; 0 <= ref2 ? o12 <= ref2 : o12 >= ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
                  push(stack[polycoeff + i5]);
                  push(variable);
                  push_integer(i5);
                  power();
                  multiply();
                  add();
                }
                remainingPoly = pop();
              }
            } else if (whichRootsAreWeFinding === "complex") {
              if (foundComplexRoot === 0) {
                break;
              } else {
                push(A2);
                push(variable);
                subtract();
                push(A2);
                conjugate();
                push(variable);
                subtract();
                multiply();
                secondDegreePloly = pop();
                if (DEBUG) {
                  console.log("yyfactorpoly: " + firstParam + " " + secondParam + " success\nFACTOR=" + secondDegreePloly);
                }
                push(partOfPolynomialFactoredSoFar);
                previousFactorisation = pop();
                push(partOfPolynomialFactoredSoFar);
                push(secondDegreePloly);
                multiply_noexpand();
                partOfPolynomialFactoredSoFar = pop();
                if (remainingPoly == null) {
                  push(zero);
                  for (i5 = i12 = 0, ref12 = factpoly_expo; 0 <= ref12 ? i12 <= ref12 : i12 >= ref12; i5 = 0 <= ref12 ? ++i12 : --i12) {
                    push(stack[polycoeff + i5]);
                    push(variable);
                    push_integer(i5);
                    power();
                    multiply();
                    add();
                  }
                  remainingPoly = pop();
                }
                dividend = remainingPoly;
                push(dividend);
                push(secondDegreePloly);
                push(variable);
                divpoly();
                remainingPoly = pop();
                push(remainingPoly);
                push(secondDegreePloly);
                multiply();
                checkingTheDivision = pop();
                if (!equal(checkingTheDivision, dividend)) {
                  if (DEBUG) {
                    console.log("we found a polynomial based on complex root and its conj but it doesn't divide the poly, quitting");
                  }
                  if (DEBUG) {
                    console.log("so just returning previousFactorisation times dividend: " + previousFactorisation + " * " + dividend);
                  }
                  push(previousFactorisation);
                  push(dividend);
                  prev_expanding = expanding;
                  expanding = 0;
                  yycondense();
                  expanding = prev_expanding;
                  multiply_noexpand();
                  partOfPolynomialFactoredSoFar = pop();
                  stack[h5] = partOfPolynomialFactoredSoFar;
                  moveTos(h5 + 1);
                  restore();
                  return;
                }
                for (i5 = j12 = 0, ref22 = factpoly_expo; 0 <= ref22 ? j12 <= ref22 : j12 >= ref22; i5 = 0 <= ref22 ? ++j12 : --j12) {
                  pop();
                }
                coeff(variable, remainingPoly);
                factpoly_expo -= 2;
              }
            }
          }
          if (DEBUG) {
            console.log("yyfactorpoly: " + firstParam + " " + secondParam + " building the remaining unfactored part of the polynomial");
          }
          push(zero);
          for (i5 = l1 = 0, ref3 = factpoly_expo; 0 <= ref3 ? l1 <= ref3 : l1 >= ref3; i5 = 0 <= ref3 ? ++l1 : --l1) {
            push(stack[polycoeff + i5]);
            push(variable);
            push_integer(i5);
            power();
            multiply();
            add();
          }
          polynomial = pop();
          if (DEBUG) {
            console.log("yyfactorpoly: " + firstParam + " " + secondParam + " remaining unfactored part of the polynomial: " + polynomial.toString());
          }
          push(polynomial);
          prev_expanding = expanding;
          expanding = 0;
          yycondense();
          expanding = prev_expanding;
          polynomial = pop();
          if (DEBUG) {
            console.log("yyfactorpoly: " + firstParam + " " + secondParam + " new poly with extracted common factor: " + polynomial.toString());
          }
          if (factpoly_expo > 0 && isnegativeterm(stack[polycoeff + factpoly_expo])) {
            push(polynomial);
            negate();
            polynomial = pop();
            push(partOfPolynomialFactoredSoFar);
            negate_noexpand();
            partOfPolynomialFactoredSoFar = pop();
          }
          push(partOfPolynomialFactoredSoFar);
          push(polynomial);
          multiply_noexpand();
          partOfPolynomialFactoredSoFar = pop();
          if (DEBUG) {
            console.log("yyfactorpoly: " + firstParam + " " + secondParam + " result: " + partOfPolynomialFactoredSoFar);
          }
          stack[h5] = partOfPolynomialFactoredSoFar;
          moveTos(h5 + 1);
          return restore();
        };
        rationalize_coefficients = function(h5) {
          var i5, i12, o12, ratio, ratioInverse, ref2, ref12, ref22, ref3;
          ratio = one;
          for (i5 = o12 = ref2 = h5, ref12 = tos; ref2 <= ref12 ? o12 < ref12 : o12 > ref12; i5 = ref2 <= ref12 ? ++o12 : --o12) {
            push(stack[i5]);
            denominator();
            push(ratio);
            lcm();
            ratio = pop();
          }
          for (i5 = i12 = ref22 = h5, ref3 = tos; ref22 <= ref3 ? i12 < ref3 : i12 > ref3; i5 = ref22 <= ref3 ? ++i12 : --i12) {
            push(ratio);
            push(stack[i5]);
            multiply();
            stack[i5] = pop();
          }
          push(ratio);
          reciprocate();
          ratioInverse = pop();
          if (DEBUG) {
            console.log("rationalize_coefficients result: " + ratioInverse.toString());
          }
          return ratioInverse;
        };
        get_factor_from_real_root = function(variable, factpoly_expo, polycoeff) {
          var a0, an, evalPolyResult, h5, i5, i12, j2, j12, l1, m1, na0, nan, o12, polynomial, ref2, ref12, ref22, ref3, ref4, rootsTries_i, rootsTries_j, testDenominator, testNumerator, testValue;
          if (DEBUG) {
            console.log("get_factor_from_real_root");
          }
          i5 = 0;
          j2 = 0;
          h5 = 0;
          a0 = 0;
          an = 0;
          na0 = 0;
          nan = 0;
          if (DEBUG) {
            push(zero);
            for (i5 = o12 = 0, ref2 = factpoly_expo; 0 <= ref2 ? o12 <= ref2 : o12 >= ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
              push(stack[polycoeff + i5]);
              push(variable);
              push_integer(i5);
              power();
              multiply();
              add();
            }
            polynomial = pop();
            console.log("POLY=" + polynomial);
          }
          h5 = tos;
          an = tos;
          push(stack[polycoeff + factpoly_expo]);
          divisors_onstack();
          nan = tos - an;
          a0 = tos;
          push(stack[polycoeff + 0]);
          divisors_onstack();
          na0 = tos - a0;
          if (DEBUG) {
            console.log("divisors of base term");
            for (i5 = i12 = 0, ref12 = na0; 0 <= ref12 ? i12 < ref12 : i12 > ref12; i5 = 0 <= ref12 ? ++i12 : --i12) {
              console.log(", " + stack[a0 + i5]);
            }
            console.log("divisors of leading term");
            for (i5 = j12 = 0, ref22 = nan; 0 <= ref22 ? j12 < ref22 : j12 > ref22; i5 = 0 <= ref22 ? ++j12 : --j12) {
              console.log(", " + stack[an + i5]);
            }
          }
          for (rootsTries_i = l1 = 0, ref3 = nan; 0 <= ref3 ? l1 < ref3 : l1 > ref3; rootsTries_i = 0 <= ref3 ? ++l1 : --l1) {
            for (rootsTries_j = m1 = 0, ref4 = na0; 0 <= ref4 ? m1 < ref4 : m1 > ref4; rootsTries_j = 0 <= ref4 ? ++m1 : --m1) {
              testNumerator = stack[an + rootsTries_i];
              testDenominator = stack[a0 + rootsTries_j];
              push(testDenominator);
              push(testNumerator);
              divide();
              negate();
              testValue = pop();
              evalPolyResult = Evalpoly(factpoly_expo, polycoeff, testValue);
              if (DEBUG) {
                console.log("try A=" + testNumerator);
                console.log(", B=" + testDenominator);
                console.log(", root " + variable);
                console.log("=-B/A=" + testValue);
                console.log(", POLY(" + testValue);
                console.log(")=" + evalPolyResult);
              }
              if (isZeroAtomOrTensor(evalPolyResult)) {
                moveTos(h5);
                if (DEBUG) {
                  console.log("get_factor_from_real_root returning 1");
                }
                return [1, testNumerator, testDenominator];
              }
              push(testDenominator);
              negate();
              testDenominator = pop();
              push(testValue);
              negate();
              testValue = pop();
              evalPolyResult = Evalpoly(factpoly_expo, polycoeff, testValue);
              if (DEBUG) {
                console.log("try A=" + testNumerator);
                console.log(", B=" + testDenominator);
                console.log(", root " + variable);
                console.log("=-B/A=" + testValue);
                console.log(", POLY(" + testValue);
                console.log(")=" + evalPolyResult);
              }
              if (isZeroAtomOrTensor(evalPolyResult)) {
                moveTos(h5);
                if (DEBUG) {
                  console.log("get_factor_from_real_root returning 1");
                }
                return [1, testNumerator, testDenominator];
              }
            }
          }
          moveTos(h5);
          if (DEBUG) {
            console.log("get_factor_from_real_root returning");
          }
          return [0, null, null];
        };
        get_factor_from_complex_root = function(remainingPoly, factpoly_expo, polycoeff) {
          var a0, an, evalPolyResult, h5, i5, i12, j2, na0, nan, o12, rootsTries_i, rootsTries_j, testValue;
          i5 = 0;
          j2 = 0;
          h5 = 0;
          a0 = 0;
          an = 0;
          na0 = 0;
          nan = 0;
          if (factpoly_expo <= 2) {
            if (DEBUG) {
              console.log("no more factoring via complex roots to be found in polynomial of degree <= 2");
            }
            return [0, null];
          }
          if (DEBUG) {
            console.log("complex root finding for POLY=" + remainingPoly);
          }
          h5 = tos;
          an = tos;
          push_integer(-1);
          push_rational(2, 3);
          power();
          rect();
          testValue = pop();
          if (DEBUG) {
            console.log("complex root finding: trying with " + testValue);
          }
          push(testValue);
          evalPolyResult = Evalpoly(factpoly_expo, polycoeff, testValue);
          if (DEBUG) {
            console.log("complex root finding result: " + evalPolyResult);
          }
          if (isZeroAtomOrTensor(evalPolyResult)) {
            moveTos(h5);
            if (DEBUG) {
              console.log("get_factor_from_complex_root returning 1");
            }
            return [1, testValue];
          }
          push_integer(1);
          push_rational(2, 3);
          power();
          rect();
          testValue = pop();
          if (DEBUG) {
            console.log("complex root finding: trying with " + testValue);
          }
          push(testValue);
          evalPolyResult = Evalpoly(factpoly_expo, polycoeff, testValue);
          if (DEBUG) {
            console.log("complex root finding result: " + evalPolyResult);
          }
          if (isZeroAtomOrTensor(evalPolyResult)) {
            moveTos(h5);
            if (DEBUG) {
              console.log("get_factor_from_complex_root returning 1");
            }
            return [1, testValue];
          }
          for (rootsTries_i = o12 = -10; o12 <= 10; rootsTries_i = ++o12) {
            for (rootsTries_j = i12 = 1; i12 <= 5; rootsTries_j = ++i12) {
              push_integer(rootsTries_i);
              push_integer(rootsTries_j);
              push(imaginaryunit);
              multiply();
              add();
              rect();
              testValue = pop();
              if (DEBUG) {
                console.log("complex root finding: trying simple complex combination " + testValue);
              }
              push(testValue);
              evalPolyResult = Evalpoly(factpoly_expo, polycoeff, testValue);
              if (isZeroAtomOrTensor(evalPolyResult)) {
                moveTos(h5);
                if (DEBUG) {
                  console.log("found complex root: " + evalPolyResult);
                }
                return [1, testValue];
              }
            }
          }
          moveTos(h5);
          if (DEBUG) {
            console.log("get_factor_from_complex_root returning 0");
          }
          return [0, null];
        };
        yydivpoly = function(factpoly_expo, polycoeff, A2, B2) {
          var Q, i5, o12, ref2;
          Q = zero;
          for (i5 = o12 = ref2 = factpoly_expo; ref2 <= 0 ? o12 < 0 : o12 > 0; i5 = ref2 <= 0 ? ++o12 : --o12) {
            push(stack[polycoeff + i5]);
            stack[polycoeff + i5] = Q;
            push(A2);
            divide();
            Q = pop();
            push(stack[polycoeff + i5 - 1]);
            push(Q);
            push(B2);
            multiply();
            subtract();
            stack[polycoeff + i5 - 1] = pop();
          }
          stack[polycoeff + 0] = Q;
          if (DEBUG) {
            return console.log("yydivpoly Q: " + Q.toString());
          }
        };
        Evalpoly = function(factpoly_expo, polycoeff, evaluateAt) {
          var i5, o12, ref2;
          push(zero);
          for (i5 = o12 = ref2 = factpoly_expo; ref2 <= 0 ? o12 <= 0 : o12 >= 0; i5 = ref2 <= 0 ? ++o12 : --o12) {
            push(evaluateAt);
            multiply();
            push(stack[polycoeff + i5]);
            add();
          }
          return pop();
        };
        factors = function(p11) {
          var h5;
          h5 = tos;
          if (car(p11) === symbol(ADD)) {
            p11 = cdr(p11);
            while (iscons(p11)) {
              push_term_factors(car(p11));
              p11 = cdr(p11);
            }
          } else {
            push_term_factors(p11);
          }
          return tos - h5;
        };
        push_term_factors = function(p11) {
          var results;
          if (car(p11) === symbol(MULTIPLY)) {
            p11 = cdr(p11);
            results = [];
            while (iscons(p11)) {
              push(car(p11));
              results.push(p11 = cdr(p11));
            }
            return results;
          } else {
            return push(p11);
          }
        };
        Eval_filter = function() {
          var results;
          p1 = cdr(p1);
          push(car(p1));
          Eval();
          p1 = cdr(p1);
          results = [];
          while (iscons(p1)) {
            push(car(p1));
            Eval();
            filter();
            results.push(p1 = cdr(p1));
          }
          return results;
        };
        filter = function() {
          save();
          p2 = pop();
          p1 = pop();
          filter_main();
          return restore();
        };
        filter_main = function() {
          if (car(p1) === symbol(ADD)) {
            return filter_sum();
          } else if (istensor(p1)) {
            return filter_tensor();
          } else if (Find(p1, p2)) {
            return push_integer(0);
          } else {
            return push(p1);
          }
        };
        filter_sum = function() {
          var results;
          push_integer(0);
          p1 = cdr(p1);
          results = [];
          while (iscons(p1)) {
            push(car(p1));
            push(p2);
            filter();
            add();
            results.push(p1 = cdr(p1));
          }
          return results;
        };
        filter_tensor = function() {
          var i5, i12, n9, o12, ref2, ref12;
          i5 = 0;
          n9 = 0;
          n9 = p1.tensor.nelem;
          p3 = alloc_tensor(n9);
          p3.tensor.ndim = p1.tensor.ndim;
          for (i5 = o12 = 0, ref2 = p1.tensor.ndim; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            p3.tensor.dim[i5] = p1.tensor.dim[i5];
          }
          for (i5 = i12 = 0, ref12 = n9; 0 <= ref12 ? i12 < ref12 : i12 > ref12; i5 = 0 <= ref12 ? ++i12 : --i12) {
            push(p1.tensor.elem[i5]);
            push(p2);
            filter();
            p3.tensor.elem[i5] = pop();
          }
          return push(p3);
        };
        Eval_float = function() {
          evaluatingAsFloats++;
          push(cadr(p1));
          Eval();
          yyfloat();
          Eval();
          return evaluatingAsFloats--;
        };
        checkFloatHasWorkedOutCompletely = function(nodeToCheck) {
          var numberOfEs, numberOfMults, numberOfPIs, numberOfPowers, numberOfSums;
          numberOfPowers = countOccurrencesOfSymbol(symbol(POWER), nodeToCheck);
          numberOfPIs = countOccurrencesOfSymbol(symbol(PI), nodeToCheck);
          numberOfEs = countOccurrencesOfSymbol(symbol(E), nodeToCheck);
          numberOfMults = countOccurrencesOfSymbol(symbol(MULTIPLY), nodeToCheck);
          numberOfSums = countOccurrencesOfSymbol(symbol(ADD), nodeToCheck);
          if (DEBUG) {
            console.log("     ... numberOfPowers: " + numberOfPowers);
            console.log("     ... numberOfPIs: " + numberOfPIs);
            console.log("     ... numberOfEs: " + numberOfEs);
            console.log("     ... numberOfMults: " + numberOfMults);
            console.log("     ... numberOfSums: " + numberOfSums);
          }
          if (numberOfPowers > 1 || numberOfPIs > 0 || numberOfEs > 0 || numberOfMults > 1 || numberOfSums > 1) {
            return stop("float: some unevalued parts in " + nodeToCheck);
          }
        };
        zzfloat = function() {
          save();
          evaluatingAsFloats++;
          Eval();
          yyfloat();
          Eval();
          evaluatingAsFloats--;
          return restore();
        };
        yyfloat = function() {
          var h5, i5, o12, ref2;
          i5 = 0;
          h5 = 0;
          evaluatingAsFloats++;
          save();
          p1 = pop();
          if (iscons(p1)) {
            h5 = tos;
            while (iscons(p1)) {
              push(car(p1));
              yyfloat();
              p1 = cdr(p1);
            }
            list(tos - h5);
          } else if (p1.k === TENSOR) {
            push(p1);
            copy_tensor();
            p1 = pop();
            for (i5 = o12 = 0, ref2 = p1.tensor.nelem; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
              push(p1.tensor.elem[i5]);
              yyfloat();
              p1.tensor.elem[i5] = pop();
            }
            push(p1);
          } else if (p1.k === NUM) {
            push(p1);
            bignum_float();
          } else if (p1 === symbol(PI)) {
            push_double(Math.PI);
          } else if (p1 === symbol(E)) {
            push_double(Math.E);
          } else {
            push(p1);
          }
          restore();
          return evaluatingAsFloats--;
        };
        Eval_floor = function() {
          push(cadr(p1));
          Eval();
          return yfloor();
        };
        yfloor = function() {
          save();
          yyfloor();
          return restore();
        };
        yyfloor = function() {
          var d3;
          d3 = 0;
          p1 = pop();
          if (!isNumericAtom(p1)) {
            push_symbol(FLOOR);
            push(p1);
            list(2);
            return;
          }
          if (isdouble(p1)) {
            d3 = Math.floor(p1.d);
            push_double(d3);
            return;
          }
          if (isinteger(p1)) {
            push(p1);
            return;
          }
          p3 = new U();
          p3.k = NUM;
          p3.q.a = mdiv(p1.q.a, p1.q.b);
          p3.q.b = mint(1);
          push(p3);
          if (isnegativenumber(p1)) {
            push_integer(-1);
            return add();
          }
        };
        Eval_for = function() {
          var i5, j2, k2, loopingVariable, o12, ref2, ref12;
          i5 = 0;
          j2 = 0;
          k2 = 0;
          loopingVariable = caddr(p1);
          if (!issymbol(loopingVariable)) {
            stop("for: 2nd arg should be the variable to loop over");
          }
          push(cadddr(p1));
          Eval();
          j2 = pop_integer();
          if (isNaN(j2)) {
            push(p1);
            return;
          }
          push(caddddr(p1));
          Eval();
          k2 = pop_integer();
          if (isNaN(k2)) {
            push(p1);
            return;
          }
          p4 = get_binding(loopingVariable);
          for (i5 = o12 = ref2 = j2, ref12 = k2; ref2 <= ref12 ? o12 <= ref12 : o12 >= ref12; i5 = ref2 <= ref12 ? ++o12 : --o12) {
            push_integer(i5);
            p5 = pop();
            set_binding(loopingVariable, p5);
            push(cadr(p1));
            Eval();
            pop();
          }
          set_binding(loopingVariable, p4);
          return push_symbol(NIL);
        };
        Eval_gamma = function() {
          push(cadr(p1));
          Eval();
          return gamma();
        };
        gamma = function() {
          save();
          gammaf();
          return restore();
        };
        gammaf = function() {
          p1 = pop();
          if (isrational(p1) && MEQUAL(p1.q.a, 1) && MEQUAL(p1.q.b, 2)) {
            if (evaluatingAsFloats) {
              push_double(Math.PI);
            } else {
              push_symbol(PI);
            }
            push_rational(1, 2);
            power();
            return;
          }
          if (isrational(p1) && MEQUAL(p1.q.a, 3) && MEQUAL(p1.q.b, 2)) {
            if (evaluatingAsFloats) {
              push_double(Math.PI);
            } else {
              push_symbol(PI);
            }
            push_rational(1, 2);
            power();
            push_rational(1, 2);
            multiply();
            return;
          }
          if (isnegativeterm(p1)) {
            if (evaluatingAsFloats) {
              push_double(Math.PI);
            } else {
              push_symbol(PI);
            }
            push_integer(-1);
            multiply();
            if (evaluatingAsFloats) {
              push_double(Math.PI);
            } else {
              push_symbol(PI);
            }
            push(p1);
            multiply();
            sine();
            push(p1);
            multiply();
            push(p1);
            negate();
            gamma();
            multiply();
            divide();
            return;
          }
          if (car(p1) === symbol(ADD)) {
            gamma_of_sum();
            return;
          }
          push_symbol(GAMMA);
          push(p1);
          list(2);
        };
        gamma_of_sum = function() {
          p3 = cdr(p1);
          if (isrational(car(p3)) && MEQUAL(car(p3).q.a, 1) && MEQUAL(car(p3).q.b, 1)) {
            push(cadr(p3));
            push(cadr(p3));
            gamma();
            return multiply();
          } else {
            if (isrational(car(p3)) && MEQUAL(car(p3).q.a, -1) && MEQUAL(car(p3).q.b, 1)) {
              push(cadr(p3));
              gamma();
              push(cadr(p3));
              push_integer(-1);
              add();
              return divide();
            } else {
              push_symbol(GAMMA);
              push(p1);
              list(2);
            }
          }
        };
        Eval_gcd = function() {
          var results;
          p1 = cdr(p1);
          push(car(p1));
          Eval();
          p1 = cdr(p1);
          results = [];
          while (iscons(p1)) {
            push(car(p1));
            Eval();
            gcd();
            results.push(p1 = cdr(p1));
          }
          return results;
        };
        gcd = function() {
          var prev_expanding;
          prev_expanding = expanding;
          save();
          gcd_main();
          restore();
          return expanding = prev_expanding;
        };
        gcd_main = function() {
          var polyVar;
          expanding = 1;
          p2 = pop();
          p1 = pop();
          if (DEBUG) {
            console.log("gcd_main: p1: " + p1 + " p2: " + p2);
          }
          if (equal(p1, p2)) {
            push(p1);
            return;
          }
          if (isrational(p1) && isrational(p2)) {
            push(p1);
            push(p2);
            gcd_numbers();
            return;
          }
          if (polyVar = areunivarpolysfactoredorexpandedform(p1, p2)) {
            gcd_polys(polyVar);
            return;
          }
          if (car(p1) === symbol(ADD) && car(p2) === symbol(ADD)) {
            gcd_sum_sum();
            return;
          }
          if (car(p1) === symbol(ADD)) {
            gcd_sum(p1);
            p1 = pop();
          }
          if (car(p2) === symbol(ADD)) {
            gcd_sum(p2);
            p2 = pop();
          }
          if (car(p1) === symbol(MULTIPLY)) {
            gcd_sum_product();
            return;
          }
          if (car(p2) === symbol(MULTIPLY)) {
            gcd_product_sum();
            return;
          }
          if (car(p1) === symbol(MULTIPLY) && car(p2) === symbol(MULTIPLY)) {
            gcd_product_product();
            return;
          }
          return gcd_powers_with_same_base();
        };
        areunivarpolysfactoredorexpandedform = function(p12, p22) {
          var polyVar;
          if (DEBUG) {
            console.log("areunivarpolysfactoredorexpandedform: p1: " + p12 + " p2: " + p22);
          }
          if (polyVar = isunivarpolyfactoredorexpandedform(p12)) {
            if (isunivarpolyfactoredorexpandedform(p22, polyVar)) {
              return polyVar;
            }
          }
          return false;
        };
        gcd_polys = function(polyVar) {
          if (DEBUG) {
            console.log("gcd_polys: p1: " + p1 + " polyVar: " + polyVar);
          }
          push(p1);
          push(polyVar);
          factorpoly();
          p1 = pop();
          push(p2);
          push(polyVar);
          factorpoly();
          p2 = pop();
          if (DEBUG) {
            console.log("GCD: factored polys:");
          }
          if (DEBUG) {
            console.log("  p1:" + p1.toString());
          }
          if (DEBUG) {
            console.log("  p2:" + p2.toString());
          }
          if (car(p1) === symbol(MULTIPLY) || car(p2) === symbol(MULTIPLY)) {
            if (car(p1) !== symbol(MULTIPLY)) {
              push_symbol(MULTIPLY);
              push(p1);
              push(one);
              list(3);
              p1 = pop();
            }
            if (car(p2) !== symbol(MULTIPLY)) {
              push_symbol(MULTIPLY);
              push(p2);
              push(one);
              list(3);
              p2 = pop();
            }
          }
          if (car(p1) === symbol(MULTIPLY) && car(p2) === symbol(MULTIPLY)) {
            gcd_product_product();
            return;
          }
          gcd_powers_with_same_base();
          return true;
        };
        gcd_product_product = function() {
          var results;
          push(one);
          p3 = cdr(p1);
          results = [];
          while (iscons(p3)) {
            p4 = cdr(p2);
            while (iscons(p4)) {
              push(car(p3));
              push(car(p4));
              gcd();
              multiply();
              p4 = cdr(p4);
            }
            results.push(p3 = cdr(p3));
          }
          return results;
        };
        gcd_powers_with_same_base = function() {
          if (car(p1) === symbol(POWER)) {
            p3 = caddr(p1);
            p1 = cadr(p1);
          } else {
            p3 = one;
          }
          if (car(p2) === symbol(POWER)) {
            p4 = caddr(p2);
            p2 = cadr(p2);
          } else {
            p4 = one;
          }
          if (!equal(p1, p2)) {
            push(one);
            return;
          }
          if (isNumericAtom(p3) && isNumericAtom(p4)) {
            push(p1);
            if (lessp(p3, p4)) {
              push(p3);
            } else {
              push(p4);
            }
            power();
            return;
          }
          push(p3);
          push(p4);
          divide();
          p5 = pop();
          if (isNumericAtom(p5)) {
            push(p1);
            if (car(p3) === symbol(MULTIPLY) && isNumericAtom(cadr(p3))) {
              p5 = cadr(p3);
            } else {
              p5 = one;
            }
            if (car(p4) === symbol(MULTIPLY) && isNumericAtom(cadr(p4))) {
              p6 = cadr(p4);
            } else {
              p6 = one;
            }
            if (lessp(p5, p6)) {
              push(p3);
            } else {
              push(p4);
            }
            power();
            return;
          }
          push(p3);
          push(p4);
          subtract();
          p5 = pop();
          if (!isNumericAtom(p5)) {
            push(one);
            return;
          }
          push(p1);
          if (isnegativenumber(p5)) {
            push(p3);
          } else {
            push(p4);
          }
          return power();
        };
        gcd_sum_sum = function() {
          if (length(p1) !== length(p2)) {
            push(one);
            return;
          }
          p3 = cdr(p1);
          push(car(p3));
          p3 = cdr(p3);
          while (iscons(p3)) {
            push(car(p3));
            gcd();
            p3 = cdr(p3);
          }
          p3 = pop();
          p4 = cdr(p2);
          push(car(p4));
          p4 = cdr(p4);
          while (iscons(p4)) {
            push(car(p4));
            gcd();
            p4 = cdr(p4);
          }
          p4 = pop();
          push(p1);
          push(p3);
          divide();
          p5 = pop();
          push(p2);
          push(p4);
          divide();
          p6 = pop();
          if (equal(p5, p6)) {
            push(p5);
            push(p3);
            push(p4);
            gcd();
            return multiply();
          } else {
            return push(one);
          }
        };
        gcd_sum = function(p11) {
          var results;
          p11 = cdr(p11);
          push(car(p11));
          p11 = cdr(p11);
          results = [];
          while (iscons(p11)) {
            push(car(p11));
            gcd();
            results.push(p11 = cdr(p11));
          }
          return results;
        };
        gcd_sum_product = function() {
          var results;
          push(one);
          p3 = cdr(p1);
          results = [];
          while (iscons(p3)) {
            push(car(p3));
            push(p2);
            gcd();
            multiply();
            results.push(p3 = cdr(p3));
          }
          return results;
        };
        gcd_product_sum = function() {
          var results;
          push(one);
          p4 = cdr(p2);
          results = [];
          while (iscons(p4)) {
            push(p1);
            push(car(p4));
            gcd();
            multiply();
            results.push(p4 = cdr(p4));
          }
          return results;
        };
        guess = function() {
          var p11;
          p11 = pop();
          push(p11);
          if (Find(p11, symbol(SYMBOL_X))) {
            return push_symbol(SYMBOL_X);
          } else if (Find(p11, symbol(SYMBOL_Y))) {
            return push_symbol(SYMBOL_Y);
          } else if (Find(p11, symbol(SYMBOL_Z))) {
            return push_symbol(SYMBOL_Z);
          } else if (Find(p11, symbol(SYMBOL_T))) {
            return push_symbol(SYMBOL_T);
          } else if (Find(p11, symbol(SYMBOL_S))) {
            return push_symbol(SYMBOL_S);
          } else {
            return push_symbol(SYMBOL_X);
          }
        };
        hermite = function() {
          save();
          yyhermite();
          return restore();
        };
        yyhermite = function() {
          var n9;
          n9 = 0;
          p2 = pop();
          p1 = pop();
          push(p2);
          n9 = pop_integer();
          if (n9 < 0 || isNaN(n9)) {
            push_symbol(HERMITE);
            push(p1);
            push(p2);
            list(3);
            return;
          }
          if (issymbol(p1)) {
            return yyhermite2(n9);
          } else {
            p3 = p1;
            p1 = symbol(SECRETX);
            yyhermite2(n9);
            p1 = p3;
            push(symbol(SECRETX));
            push(p1);
            subst();
            return Eval();
          }
        };
        yyhermite2 = function(n9) {
          var i5, o12, ref2, results;
          i5 = 0;
          push_integer(1);
          push_integer(0);
          p4 = pop();
          results = [];
          for (i5 = o12 = 0, ref2 = n9; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            p5 = p4;
            p4 = pop();
            push(p1);
            push(p4);
            multiply();
            push_integer(i5);
            push(p5);
            multiply();
            subtract();
            push_integer(2);
            results.push(multiply());
          }
          return results;
        };
        hilbert = function() {
          var i5, i12, j2, n9, o12, ref2, ref12;
          i5 = 0;
          j2 = 0;
          n9 = 0;
          save();
          p2 = pop();
          push(p2);
          n9 = pop_integer();
          if (n9 < 2) {
            push_symbol(HILBERT);
            push(p2);
            list(2);
            restore();
            return;
          }
          push_zero_matrix(n9, n9);
          p1 = pop();
          for (i5 = o12 = 0, ref2 = n9; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            for (j2 = i12 = 0, ref12 = n9; 0 <= ref12 ? i12 < ref12 : i12 > ref12; j2 = 0 <= ref12 ? ++i12 : --i12) {
              push_integer(i5 + j2 + 1);
              inverse();
              p1.tensor.elem[i5 * n9 + j2] = pop();
            }
          }
          push(p1);
          return restore();
        };
        DEBUG_IMAG = false;
        Eval_imag = function() {
          push(cadr(p1));
          Eval();
          return imag();
        };
        imag = function() {
          save();
          rect();
          p1 = pop();
          if (DEBUG_IMAG) {
            console.log("IMAGE of " + p1);
          }
          push(p1);
          push(p1);
          conjugate();
          if (DEBUG_IMAG) {
            console.log(" image: conjugate result: " + stack[tos - 1]);
          }
          subtract();
          push_integer(2);
          divide();
          if (DEBUG_IMAG) {
            console.log(" image: 1st divide result: " + stack[tos - 1]);
          }
          push(imaginaryunit);
          divide();
          if (DEBUG_IMAG) {
            console.log(" image: 2nd divide result: " + stack[tos - 1]);
          }
          return restore();
        };
        index_function = function(n9) {
          var i5, i12, j12, k2, l1, m3, m1, ndim, nelem, o12, ref2, ref12, ref22, ref3, ref4, ref5, ref6, ref7, s7, t5;
          i5 = 0;
          k2 = 0;
          m3 = 0;
          ndim = 0;
          nelem = 0;
          t5 = 0;
          save();
          s7 = tos - n9;
          p1 = stack[s7];
          ndim = p1.tensor.ndim;
          m3 = n9 - 1;
          if (m3 > ndim) {
            stop("too many indices for tensor");
          }
          k2 = 0;
          for (i5 = o12 = 0, ref2 = m3; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            push(stack[s7 + i5 + 1]);
            t5 = pop_integer();
            if (t5 < 1 || t5 > p1.tensor.dim[i5]) {
              stop("index out of range");
            }
            k2 = k2 * p1.tensor.dim[i5] + t5 - 1;
          }
          if (ndim === m3) {
            moveTos(tos - n9);
            push(p1.tensor.elem[k2]);
            restore();
            return;
          }
          for (i5 = i12 = ref12 = m3, ref22 = ndim; ref12 <= ref22 ? i12 < ref22 : i12 > ref22; i5 = ref12 <= ref22 ? ++i12 : --i12) {
            k2 = k2 * p1.tensor.dim[i5] + 0;
          }
          nelem = 1;
          for (i5 = j12 = ref3 = m3, ref4 = ndim; ref3 <= ref4 ? j12 < ref4 : j12 > ref4; i5 = ref3 <= ref4 ? ++j12 : --j12) {
            nelem *= p1.tensor.dim[i5];
          }
          p2 = alloc_tensor(nelem);
          p2.tensor.ndim = ndim - m3;
          for (i5 = l1 = ref5 = m3, ref6 = ndim; ref5 <= ref6 ? l1 < ref6 : l1 > ref6; i5 = ref5 <= ref6 ? ++l1 : --l1) {
            p2.tensor.dim[i5 - m3] = p1.tensor.dim[i5];
          }
          for (i5 = m1 = 0, ref7 = nelem; 0 <= ref7 ? m1 < ref7 : m1 > ref7; i5 = 0 <= ref7 ? ++m1 : --m1) {
            p2.tensor.elem[i5] = p1.tensor.elem[k2 + i5];
          }
          check_tensor_dimensions(p1);
          check_tensor_dimensions(p2);
          moveTos(tos - n9);
          push(p2);
          return restore();
        };
        set_component = function(n9) {
          var i5, i12, j12, k2, l1, m3, m1, n1, ndim, o12, ref2, ref12, ref22, ref3, ref4, ref5, ref6, s7, t5;
          i5 = 0;
          k2 = 0;
          m3 = 0;
          ndim = 0;
          t5 = 0;
          save();
          if (n9 < 3) {
            stop("error in indexed assign");
          }
          s7 = tos - n9;
          p2 = stack[s7];
          p1 = stack[s7 + 1];
          if (!istensor(p1)) {
            stop("error in indexed assign: assigning to something that is not a tensor");
          }
          ndim = p1.tensor.ndim;
          m3 = n9 - 2;
          if (m3 > ndim) {
            stop("error in indexed assign");
          }
          k2 = 0;
          for (i5 = o12 = 0, ref2 = m3; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            push(stack[s7 + i5 + 2]);
            t5 = pop_integer();
            if (t5 < 1 || t5 > p1.tensor.dim[i5]) {
              stop("error in indexed assign\n");
            }
            k2 = k2 * p1.tensor.dim[i5] + t5 - 1;
          }
          for (i5 = i12 = ref12 = m3, ref22 = ndim; ref12 <= ref22 ? i12 < ref22 : i12 > ref22; i5 = ref12 <= ref22 ? ++i12 : --i12) {
            k2 = k2 * p1.tensor.dim[i5] + 0;
          }
          p3 = alloc_tensor(p1.tensor.nelem);
          p3.tensor.ndim = p1.tensor.ndim;
          for (i5 = j12 = 0, ref3 = p1.tensor.ndim; 0 <= ref3 ? j12 < ref3 : j12 > ref3; i5 = 0 <= ref3 ? ++j12 : --j12) {
            p3.tensor.dim[i5] = p1.tensor.dim[i5];
          }
          for (i5 = l1 = 0, ref4 = p1.tensor.nelem; 0 <= ref4 ? l1 < ref4 : l1 > ref4; i5 = 0 <= ref4 ? ++l1 : --l1) {
            p3.tensor.elem[i5] = p1.tensor.elem[i5];
          }
          check_tensor_dimensions(p1);
          check_tensor_dimensions(p3);
          p1 = p3;
          if (ndim === m3) {
            if (istensor(p2)) {
              stop("error in indexed assign");
            }
            p1.tensor.elem[k2] = p2;
            check_tensor_dimensions(p1);
            moveTos(tos - n9);
            push(p1);
            restore();
            return;
          }
          if (!istensor(p2)) {
            stop("error in indexed assign");
          }
          if (ndim - m3 !== p2.tensor.ndim) {
            stop("error in indexed assign");
          }
          for (i5 = m1 = 0, ref5 = p2.tensor.ndim; 0 <= ref5 ? m1 < ref5 : m1 > ref5; i5 = 0 <= ref5 ? ++m1 : --m1) {
            if (p1.tensor.dim[m3 + i5] !== p2.tensor.dim[i5]) {
              stop("error in indexed assign");
            }
          }
          for (i5 = n1 = 0, ref6 = p2.tensor.nelem; 0 <= ref6 ? n1 < ref6 : n1 > ref6; i5 = 0 <= ref6 ? ++n1 : --n1) {
            p1.tensor.elem[k2 + i5] = p2.tensor.elem[i5];
          }
          check_tensor_dimensions(p1);
          check_tensor_dimensions(p2);
          moveTos(tos - n9);
          push(p1);
          return restore();
        };
        Eval_inner = function() {
          var difference, i5, i12, j12, l1, moretheArguments, o12, operands, ref2, ref12, ref22, ref3, refinedOperands, results, secondArgument, shift, theArguments;
          theArguments = [];
          theArguments.push(car(cdr(p1)));
          secondArgument = car(cdr(cdr(p1)));
          if (secondArgument === symbol(NIL)) {
            stop("pattern needs at least a template and a transformed version");
          }
          moretheArguments = cdr(cdr(p1));
          while (moretheArguments !== symbol(NIL)) {
            theArguments.push(car(moretheArguments));
            moretheArguments = cdr(moretheArguments);
          }
          if (theArguments.length > 2) {
            push_symbol(INNER);
            push(theArguments[theArguments.length - 2]);
            push(theArguments[theArguments.length - 1]);
            list(3);
            for (i5 = o12 = 2, ref2 = theArguments.length; 2 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 2 <= ref2 ? ++o12 : --o12) {
              push_symbol(INNER);
              swap();
              push(theArguments[theArguments.length - i5 - 1]);
              swap();
              list(3);
            }
            p1 = pop();
            Eval_inner();
            return;
          }
          operands = [];
          get_innerprod_factors(p1, operands);
          refinedOperands = [];
          for (i5 = i12 = 0, ref12 = operands.length; 0 <= ref12 ? i12 < ref12 : i12 > ref12; i5 = 0 <= ref12 ? ++i12 : --i12) {
            if (operands[i5] === symbol(SYMBOL_IDENTITY_MATRIX)) {
              continue;
            } else {
              refinedOperands.push(operands[i5]);
            }
          }
          operands = refinedOperands;
          refinedOperands = [];
          if (operands.length > 1) {
            shift = 0;
            for (i5 = j12 = 0, ref22 = operands.length; 0 <= ref22 ? j12 < ref22 : j12 > ref22; i5 = 0 <= ref22 ? ++j12 : --j12) {
              if (i5 + shift + 1 <= operands.length - 1) {
                if (!(isNumericAtomOrTensor(operands[i5 + shift]) || isNumericAtomOrTensor(operands[i5 + shift + 1]))) {
                  push(operands[i5 + shift]);
                  Eval();
                  inv();
                  push(operands[i5 + shift + 1]);
                  Eval();
                  subtract();
                  difference = pop();
                  if (isZeroAtomOrTensor(difference)) {
                    shift += 1;
                  } else {
                    refinedOperands.push(operands[i5 + shift]);
                  }
                } else {
                  refinedOperands.push(operands[i5 + shift]);
                }
              } else {
                break;
              }
              if (i5 + shift === operands.length - 2) {
                refinedOperands.push(operands[operands.length - 1]);
              }
              if (i5 + shift >= operands.length - 1) {
                break;
              }
            }
            operands = refinedOperands;
          }
          push(symbol(INNER));
          if (operands.length > 0) {
            for (i5 = l1 = 0, ref3 = operands.length; 0 <= ref3 ? l1 < ref3 : l1 > ref3; i5 = 0 <= ref3 ? ++l1 : --l1) {
              push(operands[i5]);
            }
          } else {
            pop();
            push(symbol(SYMBOL_IDENTITY_MATRIX));
            return;
          }
          list(operands.length + 1);
          p1 = pop();
          p1 = cdr(p1);
          push(car(p1));
          Eval();
          p1 = cdr(p1);
          results = [];
          while (iscons(p1)) {
            push(car(p1));
            Eval();
            inner();
            results.push(p1 = cdr(p1));
          }
          return results;
        };
        inner = function() {
          var arg1, arg2, arg3, subtractionResult;
          save();
          p2 = pop();
          p1 = pop();
          if (isnegativeterm(p2) && isnegativeterm(p1)) {
            push(p2);
            negate();
            p2 = pop();
            push(p1);
            negate();
            p1 = pop();
          }
          if (isinnerordot(p1)) {
            arg1 = car(cdr(p1));
            arg2 = car(cdr(cdr(p1)));
            arg3 = p2;
            p1 = arg1;
            push(arg2);
            push(arg3);
            inner();
            p2 = pop();
          }
          if (p1 === symbol(SYMBOL_IDENTITY_MATRIX)) {
            push(p2);
            restore();
            return;
          } else if (p2 === symbol(SYMBOL_IDENTITY_MATRIX)) {
            push(p1);
            restore();
            return;
          }
          if (istensor(p1) && istensor(p2)) {
            inner_f();
          } else {
            if (!(isNumericAtomOrTensor(p1) || isNumericAtomOrTensor(p2))) {
              push(p1);
              push(p2);
              inv();
              subtract();
              subtractionResult = pop();
              if (isZeroAtomOrTensor(subtractionResult)) {
                push_symbol(SYMBOL_IDENTITY_MATRIX);
                restore();
                return;
              }
            }
            if (expanding && isadd(p1)) {
              p1 = cdr(p1);
              push(zero);
              while (iscons(p1)) {
                push(car(p1));
                push(p2);
                inner();
                add();
                p1 = cdr(p1);
              }
              restore();
              return;
            }
            if (expanding && isadd(p2)) {
              p2 = cdr(p2);
              push(zero);
              while (iscons(p2)) {
                push(p1);
                push(car(p2));
                inner();
                add();
                p2 = cdr(p2);
              }
              restore();
              return;
            }
            push(p1);
            push(p2);
            if (istensor(p1) && isNumericAtom(p2)) {
              tensor_times_scalar();
            } else if (isNumericAtom(p1) && istensor(p2)) {
              scalar_times_tensor();
            } else {
              if (isNumericAtom(p1) || isNumericAtom(p2)) {
                multiply();
              } else {
                pop();
                pop();
                push_symbol(INNER);
                push(p1);
                push(p2);
                list(3);
                restore();
                return;
              }
            }
          }
          return restore();
        };
        inner_f = function() {
          var a4, ak, b2, bk, c6, i5, i12, j2, j12, k2, l1, m1, n9, n1, ndim, o12, o1, ref2, ref12, ref22, ref3, ref4, ref5, ref6;
          i5 = 0;
          n9 = p1.tensor.dim[p1.tensor.ndim - 1];
          if (n9 !== p2.tensor.dim[0]) {
            debugger;
            stop("inner: tensor dimension check");
          }
          ndim = p1.tensor.ndim + p2.tensor.ndim - 2;
          if (ndim > MAXDIM) {
            stop("inner: rank of result exceeds maximum");
          }
          a4 = p1.tensor.elem;
          b2 = p2.tensor.elem;
          ak = 1;
          for (i5 = o12 = 0, ref2 = p1.tensor.ndim - 1; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            ak *= p1.tensor.dim[i5];
          }
          bk = 1;
          for (i5 = i12 = 1, ref12 = p2.tensor.ndim; 1 <= ref12 ? i12 < ref12 : i12 > ref12; i5 = 1 <= ref12 ? ++i12 : --i12) {
            bk *= p2.tensor.dim[i5];
          }
          p3 = alloc_tensor(ak * bk);
          c6 = p3.tensor.elem;
          for (i5 = j12 = 0, ref22 = ak; 0 <= ref22 ? j12 < ref22 : j12 > ref22; i5 = 0 <= ref22 ? ++j12 : --j12) {
            for (j2 = l1 = 0, ref3 = n9; 0 <= ref3 ? l1 < ref3 : l1 > ref3; j2 = 0 <= ref3 ? ++l1 : --l1) {
              if (isZeroAtomOrTensor(a4[i5 * n9 + j2])) {
                continue;
              }
              for (k2 = m1 = 0, ref4 = bk; 0 <= ref4 ? m1 < ref4 : m1 > ref4; k2 = 0 <= ref4 ? ++m1 : --m1) {
                push(a4[i5 * n9 + j2]);
                push(b2[j2 * bk + k2]);
                multiply();
                push(c6[i5 * bk + k2]);
                add();
                c6[i5 * bk + k2] = pop();
              }
            }
          }
          if (ndim === 0) {
            return push(p3.tensor.elem[0]);
          } else {
            p3.tensor.ndim = ndim;
            j2 = 0;
            for (i5 = n1 = 0, ref5 = p1.tensor.ndim - 1; 0 <= ref5 ? n1 < ref5 : n1 > ref5; i5 = 0 <= ref5 ? ++n1 : --n1) {
              p3.tensor.dim[i5] = p1.tensor.dim[i5];
            }
            j2 = p1.tensor.ndim - 1;
            for (i5 = o1 = 0, ref6 = p2.tensor.ndim - 1; 0 <= ref6 ? o1 < ref6 : o1 > ref6; i5 = 0 <= ref6 ? ++o1 : --o1) {
              p3.tensor.dim[j2 + i5] = p2.tensor.dim[i5 + 1];
            }
            return push(p3);
          }
        };
        get_innerprod_factors = function(tree, factors_accumulator) {
          if (!iscons(tree)) {
            add_factor_to_accumulator(tree, factors_accumulator);
            return;
          }
          if (cdr(tree) === symbol(NIL)) {
            tree = get_innerprod_factors(car(tree), factors_accumulator);
            return;
          }
          if (isinnerordot(tree)) {
            get_innerprod_factors(car(cdr(tree)), factors_accumulator);
            get_innerprod_factors(cdr(cdr(tree)), factors_accumulator);
            return;
          }
          return add_factor_to_accumulator(tree, factors_accumulator);
        };
        add_factor_to_accumulator = function(tree, factors_accumulator) {
          if (tree !== symbol(NIL)) {
            return factors_accumulator.push(tree);
          }
        };
        itab = [
          // 1
          "f(a,a*x)",
          // 9 (need a caveat for 7 so we can put 9 after 7)
          "f(1/x,log(x))",
          // 7
          "f(x^a,x^(a+1)/(a+1))",
          // five specialisations of case 7 for speed.
          // Covers often-occurring exponents: each of
          // these case ends up in a dedicated entry, so we
          // only have to do one sure-shot match.
          "f(x^(-2),-x^(-1))",
          "f(x^(-1/2),2*x^(1/2))",
          "f(x^(1/2),2/3*x^(3/2))",
          "f(x,x^2/2)",
          "f(x^2,x^3/3)",
          // 12
          "f(exp(a*x),1/a*exp(a*x))",
          "f(exp(a*x+b),1/a*exp(a*x+b))",
          "f(x*exp(a*x^2),exp(a*x^2)/(2*a))",
          "f(x*exp(a*x^2+b),exp(a*x^2+b)/(2*a))",
          // 14
          "f(log(a*x),x*log(a*x)-x)",
          // 15
          "f(a^x,a^x/log(a),or(not(number(a)),a>0))",
          // 16
          "f(1/(a+x^2),1/sqrt(a)*arctan(x/sqrt(a)),or(not(number(a)),a>0))",
          // 17
          "f(1/(a-x^2),1/sqrt(a)*arctanh(x/sqrt(a)))",
          // 19
          "f(1/sqrt(a-x^2),arcsin(x/(sqrt(a))))",
          // 20
          "f(1/sqrt(a+x^2),log(x+sqrt(a+x^2)))",
          // 27
          "f(1/(a+b*x),1/b*log(a+b*x))",
          // 28
          "f(1/(a+b*x)^2,-1/(b*(a+b*x)))",
          // 29
          "f(1/(a+b*x)^3,-1/(2*b)*1/(a+b*x)^2)",
          // 30
          "f(x/(a+b*x),x/b-a*log(a+b*x)/b/b)",
          // 31
          "f(x/(a+b*x)^2,1/b^2*(log(a+b*x)+a/(a+b*x)))",
          // 33
          "f(x^2/(a+b*x),1/b^2*(1/2*(a+b*x)^2-2*a*(a+b*x)+a^2*log(a+b*x)))",
          // 34
          "f(x^2/(a+b*x)^2,1/b^3*(a+b*x-2*a*log(a+b*x)-a^2/(a+b*x)))",
          // 35
          "f(x^2/(a+b*x)^3,1/b^3*(log(a+b*x)+2*a/(a+b*x)-1/2*a^2/(a+b*x)^2))",
          // 37
          "f(1/x*1/(a+b*x),-1/a*log((a+b*x)/x))",
          // 38
          "f(1/x*1/(a+b*x)^2,1/a*1/(a+b*x)-1/a^2*log((a+b*x)/x))",
          // 39
          "f(1/x*1/(a+b*x)^3,1/a^3*(1/2*((2*a+b*x)/(a+b*x))^2+log(x/(a+b*x))))",
          // 40
          "f(1/x^2*1/(a+b*x),-1/(a*x)+b/a^2*log((a+b*x)/x))",
          // 41
          "f(1/x^3*1/(a+b*x),(2*b*x-a)/(2*a^2*x^2)+b^2/a^3*log(x/(a+b*x)))",
          // 42
          "f(1/x^2*1/(a+b*x)^2,-(a+2*b*x)/(a^2*x*(a+b*x))+2*b/a^3*log((a+b*x)/x))",
          // 60
          "f(1/(a+b*x^2),1/sqrt(a*b)*arctan(x*sqrt(a*b)/a),or(not(number(a*b)),a*b>0))",
          // 61
          "f(1/(a+b*x^2),1/(2*sqrt(-a*b))*log((a+x*sqrt(-a*b))/(a-x*sqrt(-a*b))),or(not(number(a*b)),a*b<0))",
          // 62 is the same as 60
          // 63
          "f(x/(a+b*x^2),1/2*1/b*log(a+b*x^2))",
          //64
          "f(x^2/(a+b*x^2),x/b-a/b*integral(1/(a+b*x^2),x))",
          //65
          "f(1/(a+b*x^2)^2,x/(2*a*(a+b*x^2))+1/2*1/a*integral(1/(a+b*x^2),x))",
          //66 is covered by 61
          //70
          "f(1/x*1/(a+b*x^2),1/2*1/a*log(x^2/(a+b*x^2)))",
          //71
          "f(1/x^2*1/(a+b*x^2),-1/(a*x)-b/a*integral(1/(a+b*x^2),x))",
          //74
          "f(1/(a+b*x^3),1/3*1/a*(a/b)^(1/3)*(1/2*log(((a/b)^(1/3)+x)^3/(a+b*x^3))+sqrt(3)*arctan((2*x-(a/b)^(1/3))*(a/b)^(-1/3)/sqrt(3))))",
          //76
          "f(x^2/(a+b*x^3),1/3*1/b*log(a+b*x^3))",
          // float(defint(1/(2+3*X^4),X,0,pi)) gave wrong result.
          // Also, the tests related to the indefinite integral
          // fail since we rationalise expressions "better", so I'm thinking
          // to take this out completely as it seemed to give the
          // wrong results in the first place.
          //77
          //"f(1/(a+b*x^4),1/2*1/a*(a/b/4)^(1/4)*(1/2*log((x^2+2*(a/b/4)^(1/4)*x+2*(a/b/4)^(1/2))/(x^2-2*(a/b/4)^(1/4)*x+2*(a/b/4)^(1/2)))+arctan(2*(a/b/4)^(1/4)*x/(2*(a/b/4)^(1/2)-x^2))),or(not(number(a*b)),a*b>0))",
          //78
          //"f(1/(a+b*x^4),1/2*(-a/b)^(1/4)/a*(1/2*log((x+(-a/b)^(1/4))/(x-(-a/b)^(1/4)))+arctan(x*(-a/b)^(-1/4))),or(not(number(a*b)),a*b<0))",
          //79
          "f(x/(a+b*x^4),1/2*sqrt(b/a)/b*arctan(x^2*sqrt(b/a)),or(not(number(a*b)),a*b>0))",
          //80
          "f(x/(a+b*x^4),1/4*sqrt(-b/a)/b*log((x^2-sqrt(-a/b))/(x^2+sqrt(-a/b))),or(not(number(a*b)),a*b<0))",
          // float(defint(X^2/(2+3*X^4),X,0,pi)) gave wrong result.
          // Also, the tests related to the indefinite integral
          // fail since we rationalise expressions "better", so I'm thinking
          // to take this out completely as it seemed to give the
          // wrong results in the first place.
          //81
          //"f(x^2/(a+b*x^4),1/4*1/b*(a/b/4)^(-1/4)*(1/2*log((x^2-2*(a/b/4)^(1/4)*x+2*sqrt(a/b/4))/(x^2+2*(a/b/4)^(1/4)*x+2*sqrt(a/b/4)))+arctan(2*(a/b/4)^(1/4)*x/(2*sqrt(a/b/4)-x^2))),or(not(number(a*b)),a*b>0))",
          //82
          //"f(x^2/(a+b*x^4),1/4*1/b*(-a/b)^(-1/4)*(log((x-(-a/b)^(1/4))/(x+(-a/b)^(1/4)))+2*arctan(x*(-a/b)^(-1/4))),or(not(number(a*b)),a*b<0))",
          //83
          "f(x^3/(a+b*x^4),1/4*1/b*log(a+b*x^4))",
          //124
          "f(sqrt(a+b*x),2/3*1/b*sqrt((a+b*x)^3))",
          //125
          "f(x*sqrt(a+b*x),-2*(2*a-3*b*x)*sqrt((a+b*x)^3)/15/b^2)",
          //126
          "f(x^2*sqrt(a+b*x),2*(8*a^2-12*a*b*x+15*b^2*x^2)*sqrt((a+b*x)^3)/105/b^3)",
          //128
          "f(sqrt(a+b*x)/x,2*sqrt(a+b*x)+a*integral(1/x*1/sqrt(a+b*x),x))",
          //129
          "f(sqrt(a+b*x)/x^2,-sqrt(a+b*x)/x+b/2*integral(1/x*1/sqrt(a+b*x),x))",
          //131
          "f(1/sqrt(a+b*x),2*sqrt(a+b*x)/b)",
          //132
          "f(x/sqrt(a+b*x),-2/3*(2*a-b*x)*sqrt(a+b*x)/b^2)",
          //133
          "f(x^2/sqrt(a+b*x),2/15*(8*a^2-4*a*b*x+3*b^2*x^2)*sqrt(a+b*x)/b^3)",
          //135
          "f(1/x*1/sqrt(a+b*x),1/sqrt(a)*log((sqrt(a+b*x)-sqrt(a))/(sqrt(a+b*x)+sqrt(a))),or(not(number(a)),a>0))",
          //136
          "f(1/x*1/sqrt(a+b*x),2/sqrt(-a)*arctan(sqrt(-(a+b*x)/a)),or(not(number(a)),a<0))",
          //137
          "f(1/x^2*1/sqrt(a+b*x),-sqrt(a+b*x)/a/x-1/2*b/a*integral(1/x*1/sqrt(a+b*x),x))",
          //156
          "f(sqrt(x^2+a),1/2*(x*sqrt(x^2+a)+a*log(x+sqrt(x^2+a))))",
          //157
          "f(1/sqrt(x^2+a),log(x+sqrt(x^2+a)))",
          //158
          "f(1/x*1/sqrt(x^2+a),arcsec(x/sqrt(-a))/sqrt(-a),or(not(number(a)),a<0))",
          //159
          "f(1/x*1/sqrt(x^2+a),-1/sqrt(a)*log((sqrt(a)+sqrt(x^2+a))/x),or(not(number(a)),a>0))",
          //160
          "f(sqrt(x^2+a)/x,sqrt(x^2+a)-sqrt(a)*log((sqrt(a)+sqrt(x^2+a))/x),or(not(number(a)),a>0))",
          //161
          "f(sqrt(x^2+a)/x,sqrt(x^2+a)-sqrt(-a)*arcsec(x/sqrt(-a)),or(not(number(a)),a<0))",
          //162
          "f(x/sqrt(x^2+a),sqrt(x^2+a))",
          //163
          "f(x*sqrt(x^2+a),1/3*sqrt((x^2+a)^3))",
          //164 need an unexpanded version?
          "f(sqrt(a+x^6+3*a^(1/3)*x^4+3*a^(2/3)*x^2),1/4*(x*sqrt((x^2+a^(1/3))^3)+3/2*a^(1/3)*x*sqrt(x^2+a^(1/3))+3/2*a^(2/3)*log(x+sqrt(x^2+a^(1/3)))))",
          // match doesn't work for the following
          "f(sqrt(-a+x^6-3*a^(1/3)*x^4+3*a^(2/3)*x^2),1/4*(x*sqrt((x^2-a^(1/3))^3)-3/2*a^(1/3)*x*sqrt(x^2-a^(1/3))+3/2*a^(2/3)*log(x+sqrt(x^2-a^(1/3)))))",
          //165
          "f(1/sqrt(a+x^6+3*a^(1/3)*x^4+3*a^(2/3)*x^2),x/a^(1/3)/sqrt(x^2+a^(1/3)))",
          //166
          "f(x/sqrt(a+x^6+3*a^(1/3)*x^4+3*a^(2/3)*x^2),-1/sqrt(x^2+a^(1/3)))",
          //167
          "f(x*sqrt(a+x^6+3*a^(1/3)*x^4+3*a^(2/3)*x^2),1/5*sqrt((x^2+a^(1/3))^5))",
          //168
          "f(x^2*sqrt(x^2+a),1/4*x*sqrt((x^2+a)^3)-1/8*a*x*sqrt(x^2+a)-1/8*a^2*log(x+sqrt(x^2+a)))",
          //169
          "f(x^3*sqrt(x^2+a),(1/5*x^2-2/15*a)*sqrt((x^2+a)^3),and(number(a),a>0))",
          //170
          "f(x^3*sqrt(x^2+a),sqrt((x^2+a)^5)/5-a*sqrt((x^2+a)^3)/3,and(number(a),a<0))",
          //171
          "f(x^2/sqrt(x^2+a),1/2*x*sqrt(x^2+a)-1/2*a*log(x+sqrt(x^2+a)))",
          //172
          "f(x^3/sqrt(x^2+a),1/3*sqrt((x^2+a)^3)-a*sqrt(x^2+a))",
          //173
          "f(1/x^2*1/sqrt(x^2+a),-sqrt(x^2+a)/a/x)",
          //174
          "f(1/x^3*1/sqrt(x^2+a),-1/2*sqrt(x^2+a)/a/x^2+1/2*log((sqrt(a)+sqrt(x^2+a))/x)/a^(3/2),or(not(number(a)),a>0))",
          //175
          "f(1/x^3*1/sqrt(x^2-a),1/2*sqrt(x^2-a)/a/x^2+1/2*1/(a^(3/2))*arcsec(x/(a^(1/2))),or(not(number(a)),a>0))",
          //176+
          "f(x^2*sqrt(a+x^6+3*a^(1/3)*x^4+3*a^(2/3)*x^2),1/6*x*sqrt((x^2+a^(1/3))^5)-1/24*a^(1/3)*x*sqrt((x^2+a^(1/3))^3)-1/16*a^(2/3)*x*sqrt(x^2+a^(1/3))-1/16*a*log(x+sqrt(x^2+a^(1/3))),or(not(number(a)),a>0))",
          //176-
          "f(x^2*sqrt(-a-3*a^(1/3)*x^4+3*a^(2/3)*x^2+x^6),1/6*x*sqrt((x^2-a^(1/3))^5)+1/24*a^(1/3)*x*sqrt((x^2-a^(1/3))^3)-1/16*a^(2/3)*x*sqrt(x^2-a^(1/3))+1/16*a*log(x+sqrt(x^2-a^(1/3))),or(not(number(a)),a>0))",
          //177+
          "f(x^3*sqrt(a+x^6+3*a^(1/3)*x^4+3*a^(2/3)*x^2),1/7*sqrt((x^2+a^(1/3))^7)-1/5*a^(1/3)*sqrt((x^2+a^(1/3))^5),or(not(number(a)),a>0))",
          //177-
          "f(x^3*sqrt(-a-3*a^(1/3)*x^4+3*a^(2/3)*x^2+x^6),1/7*sqrt((x^2-a^(1/3))^7)+1/5*a^(1/3)*sqrt((x^2-a^(1/3))^5),or(not(number(a)),a>0))",
          //196
          "f(1/(x-a)/sqrt(x^2-a^2),-sqrt(x^2-a^2)/a/(x-a))",
          //197
          "f(1/(x+a)/sqrt(x^2-a^2),sqrt(x^2-a^2)/a/(x+a))",
          //200+
          "f(sqrt(a-x^2),1/2*(x*sqrt(a-x^2)+a*arcsin(x/sqrt(abs(a)))))",
          //201    (seems to be handled somewhere else)
          //202
          "f(1/x*1/sqrt(a-x^2),-1/sqrt(a)*log((sqrt(a)+sqrt(a-x^2))/x),or(not(number(a)),a>0))",
          //203
          "f(sqrt(a-x^2)/x,sqrt(a-x^2)-sqrt(a)*log((sqrt(a)+sqrt(a-x^2))/x),or(not(number(a)),a>0))",
          //204
          "f(x/sqrt(a-x^2),-sqrt(a-x^2))",
          //205
          "f(x*sqrt(a-x^2),-1/3*sqrt((a-x^2)^3))",
          //210
          "f(x^2*sqrt(a-x^2),-x/4*sqrt((a-x^2)^3)+1/8*a*(x*sqrt(a-x^2)+a*arcsin(x/sqrt(a))),or(not(number(a)),a>0))",
          //211
          "f(x^3*sqrt(a-x^2),(-1/5*x^2-2/15*a)*sqrt((a-x^2)^3),or(not(number(a)),a>0))",
          //214
          "f(x^2/sqrt(a-x^2),-x/2*sqrt(a-x^2)+a/2*arcsin(x/sqrt(a)),or(not(number(a)),a>0))",
          //215
          "f(1/x^2*1/sqrt(a-x^2),-sqrt(a-x^2)/a/x,or(not(number(a)),a>0))",
          //216
          "f(sqrt(a-x^2)/x^2,-sqrt(a-x^2)/x-arcsin(x/sqrt(a)),or(not(number(a)),a>0))",
          //217
          "f(sqrt(a-x^2)/x^3,-1/2*sqrt(a-x^2)/x^2+1/2*log((sqrt(a)+sqrt(a-x^2))/x)/sqrt(a),or(not(number(a)),a>0))",
          //218
          "f(sqrt(a-x^2)/x^4,-1/3*sqrt((a-x^2)^3)/a/x^3,or(not(number(a)),a>0))",
          // 273
          "f(sqrt(a*x^2+b),x*sqrt(a*x^2+b)/2+b*log(x*sqrt(a)+sqrt(a*x^2+b))/2/sqrt(a),and(number(a),a>0))",
          // 274
          "f(sqrt(a*x^2+b),x*sqrt(a*x^2+b)/2+b*arcsin(x*sqrt(-a/b))/2/sqrt(-a),and(number(a),a<0))",
          // 290
          "f(sin(a*x),-cos(a*x)/a)",
          // 291
          "f(cos(a*x),sin(a*x)/a)",
          // 292
          "f(tan(a*x),-log(cos(a*x))/a)",
          // 293
          "f(1/tan(a*x),log(sin(a*x))/a)",
          // 294
          "f(1/cos(a*x),log(tan(pi/4+a*x/2))/a)",
          // 295
          "f(1/sin(a*x),log(tan(a*x/2))/a)",
          // 296
          "f(sin(a*x)^2,x/2-sin(2*a*x)/(4*a))",
          // 297
          "f(sin(a*x)^3,-cos(a*x)*(sin(a*x)^2+2)/(3*a))",
          // 298
          "f(sin(a*x)^4,3/8*x-sin(2*a*x)/(4*a)+sin(4*a*x)/(32*a))",
          // 302
          "f(cos(a*x)^2,x/2+sin(2*a*x)/(4*a))",
          // 303
          "f(cos(a*x)^3,sin(a*x)*(cos(a*x)^2+2)/(3*a))",
          // 304
          "f(cos(a*x)^4,3/8*x+sin(2*a*x)/(4*a)+sin(4*a*x)/(32*a))",
          // 308
          "f(1/sin(a*x)^2,-1/(a*tan(a*x)))",
          // 312
          "f(1/cos(a*x)^2,tan(a*x)/a)",
          // 318
          "f(sin(a*x)*cos(a*x),sin(a*x)^2/(2*a))",
          // 320
          "f(sin(a*x)^2*cos(a*x)^2,-sin(4*a*x)/(32*a)+x/8)",
          // 326
          "f(sin(a*x)/cos(a*x)^2,1/(a*cos(a*x)))",
          // 327
          "f(sin(a*x)^2/cos(a*x),(log(tan(pi/4+a*x/2))-sin(a*x))/a)",
          // 328
          "f(cos(a*x)/sin(a*x)^2,-1/(a*sin(a*x)))",
          // 329
          "f(1/(sin(a*x)*cos(a*x)),log(tan(a*x))/a)",
          // 330
          "f(1/(sin(a*x)*cos(a*x)^2),(1/cos(a*x)+log(tan(a*x/2)))/a)",
          // 331
          "f(1/(sin(a*x)^2*cos(a*x)),(log(tan(pi/4+a*x/2))-1/sin(a*x))/a)",
          // 333
          "f(1/(sin(a*x)^2*cos(a*x)^2),-2/(a*tan(2*a*x)))",
          // 335
          "f(sin(a+b*x),-cos(a+b*x)/b)",
          // 336
          "f(cos(a+b*x),sin(a+b*x)/b)",
          // 337+ (with the addition of b)
          "f(1/(b+b*sin(a*x)),-tan(pi/4-a*x/2)/a/b)",
          // 337- (with the addition of b)
          "f(1/(b-b*sin(a*x)),tan(pi/4+a*x/2)/a/b)",
          // 338 (with the addition of b)
          "f(1/(b+b*cos(a*x)),tan(a*x/2)/a/b)",
          // 339 (with the addition of b)
          "f(1/(b-b*cos(a*x)),-1/tan(a*x/2)/a/b)",
          // 340
          "f(1/(a+b*sin(x)),1/sqrt(b^2-a^2)*log((a*tan(x/2)+b-sqrt(b^2-a^2))/(a*tan(x/2)+b+sqrt(b^2-a^2))),b^2-a^2)",
          // check that b^2-a^2 is not zero
          // 341
          "f(1/(a+b*cos(x)),1/sqrt(b^2-a^2)*log((sqrt(b^2-a^2)*tan(x/2)+a+b)/(sqrt(b^2-a^2)*tan(x/2)-a-b)),b^2-a^2)",
          // check that b^2-a^2 is not zero
          // 389
          "f(x*sin(a*x),sin(a*x)/a^2-x*cos(a*x)/a)",
          // 390
          "f(x^2*sin(a*x),2*x*sin(a*x)/a^2-(a^2*x^2-2)*cos(a*x)/a^3)",
          // 393
          "f(x*cos(a*x),cos(a*x)/a^2+x*sin(a*x)/a)",
          // 394
          "f(x^2*cos(a*x),2*x*cos(a*x)/a^2+(a^2*x^2-2)*sin(a*x)/a^3)",
          // 441
          "f(arcsin(a*x),x*arcsin(a*x)+sqrt(1-a^2*x^2)/a)",
          // 442
          "f(arccos(a*x),x*arccos(a*x)-sqrt(1-a^2*x^2)/a)",
          // 443
          "f(arctan(a*x),x*arctan(a*x)-1/2*log(1+a^2*x^2)/a)",
          // 485 (with addition of a)
          // however commenting out since it's a duplicate of 14
          // "f(log(a*x),x*log(a*x)-x)",
          // 486 (with addition of a)
          "f(x*log(a*x),x^2*log(a*x)/2-x^2/4)",
          // 487 (with addition of a)
          "f(x^2*log(a*x),x^3*log(a*x)/3-1/9*x^3)",
          // 489
          "f(log(x)^2,x*log(x)^2-2*x*log(x)+2*x)",
          // 493 (with addition of a)
          "f(1/x*1/(a+log(x)),log(a+log(x)))",
          // 499
          "f(log(a*x+b),(a*x+b)*log(a*x+b)/a-x)",
          // 500
          "f(log(a*x+b)/x^2,a/b*log(x)-(a*x+b)*log(a*x+b)/b/x)",
          // 554
          "f(sinh(x),cosh(x))",
          // 555
          "f(cosh(x),sinh(x))",
          // 556
          "f(tanh(x),log(cosh(x)))",
          // 560
          "f(x*sinh(x),x*cosh(x)-sinh(x))",
          // 562
          "f(x*cosh(x),x*sinh(x)-cosh(x))",
          // 566
          "f(sinh(x)^2,sinh(2*x)/4-x/2)",
          // 569
          "f(tanh(x)^2,x-tanh(x))",
          // 572
          "f(cosh(x)^2,sinh(2*x)/4+x/2)",
          // ?
          "f(x^3*exp(a*x^2),exp(a*x^2)*(x^2/a-1/(a^2))/2)",
          // ?
          "f(x^3*exp(a*x^2+b),exp(a*x^2)*exp(b)*(x^2/a-1/(a^2))/2)",
          // ?
          "f(exp(a*x^2),-i*sqrt(pi)*erf(i*sqrt(a)*x)/sqrt(a)/2)",
          // ?
          "f(erf(a*x),x*erf(a*x)+exp(-a^2*x^2)/a/sqrt(pi))",
          // these are needed for the surface integral in the manual
          "f(x^2*(1-x^2)^(3/2),(x*sqrt(1-x^2)*(-8*x^4+14*x^2-3)+3*arcsin(x))/48)",
          "f(x^2*(1-x^2)^(5/2),(x*sqrt(1-x^2)*(48*x^6-136*x^4+118*x^2-15)+15*arcsin(x))/384)",
          "f(x^4*(1-x^2)^(3/2),(-x*sqrt(1-x^2)*(16*x^6-24*x^4+2*x^2+3)+3*arcsin(x))/128)",
          "f(x*exp(a*x),exp(a*x)*(a*x-1)/(a^2))",
          "f(x*exp(a*x+b),exp(a*x+b)*(a*x-1)/(a^2))",
          "f(x^2*exp(a*x),exp(a*x)*(a^2*x^2-2*a*x+2)/(a^3))",
          "f(x^2*exp(a*x+b),exp(a*x+b)*(a^2*x^2-2*a*x+2)/(a^3))",
          "f(x^3*exp(a*x),exp(a*x)*x^3/a-3/a*integral(x^2*exp(a*x),x))",
          "f(x^3*exp(a*x+b),exp(a*x+b)*x^3/a-3/a*integral(x^2*exp(a*x+b),x))",
          0
        ];
        Eval_integral = function() {
          var doNothing, i5, i12, n9, o12, ref2, ref12;
          i5 = 0;
          n9 = 0;
          p1 = cdr(p1);
          push(car(p1));
          Eval();
          p1 = cdr(p1);
          push(car(p1));
          Eval();
          p2 = pop();
          if (p2 === symbol(NIL)) {
            guess();
            push(symbol(NIL));
          } else if (isNumericAtom(p2)) {
            guess();
            push(p2);
          } else {
            push(p2);
            p1 = cdr(p1);
            push(car(p1));
            Eval();
          }
          p5 = pop();
          p4 = pop();
          p3 = pop();
          while (1) {
            if (isNumericAtom(p5)) {
              push(p5);
              n9 = pop_integer();
              if (isNaN(n9)) {
                stop("nth integral: check n");
              }
            } else {
              n9 = 1;
            }
            push(p3);
            if (n9 >= 0) {
              for (i5 = o12 = 0, ref2 = n9; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
                push(p4);
                integral();
              }
            } else {
              n9 = -n9;
              for (i5 = i12 = 0, ref12 = n9; 0 <= ref12 ? i12 < ref12 : i12 > ref12; i5 = 0 <= ref12 ? ++i12 : --i12) {
                push(p4);
                derivative();
              }
            }
            p3 = pop();
            if (p5 === symbol(NIL)) {
              break;
            }
            if (isNumericAtom(p5)) {
              p1 = cdr(p1);
              push(car(p1));
              Eval();
              p5 = pop();
              if (p5 === symbol(NIL)) {
                break;
              }
              if (isNumericAtom(p5)) {
                doNothing = 1;
              } else {
                p4 = p5;
                p1 = cdr(p1);
                push(car(p1));
                Eval();
                p5 = pop();
              }
            } else {
              p4 = p5;
              p1 = cdr(p1);
              push(car(p1));
              Eval();
              p5 = pop();
            }
          }
          return push(p3);
        };
        integral = function() {
          save();
          p2 = pop();
          p1 = pop();
          if (car(p1) === symbol(ADD)) {
            integral_of_sum();
          } else if (car(p1) === symbol(MULTIPLY)) {
            integral_of_product();
          } else {
            integral_of_form();
          }
          p1 = pop();
          if (Find(p1, symbol(INTEGRAL))) {
            stop("integral: sorry, could not find a solution");
          }
          push(p1);
          simplify();
          Eval();
          return restore();
        };
        integral_of_sum = function() {
          var results;
          p1 = cdr(p1);
          push(car(p1));
          push(p2);
          integral();
          p1 = cdr(p1);
          results = [];
          while (iscons(p1)) {
            push(car(p1));
            push(p2);
            integral();
            add();
            results.push(p1 = cdr(p1));
          }
          return results;
        };
        integral_of_product = function() {
          push(p1);
          push(p2);
          partition();
          p1 = pop();
          integral_of_form();
          return multiply();
        };
        integral_of_form = function() {
          var hc, tab;
          hc = italu_hashcode(p1, p2).toFixed(6);
          tab = hashed_itab[hc];
          if (!tab) {
            push_symbol(INTEGRAL);
            push(p1);
            push(p2);
            list(3);
            return;
          }
          push(p1);
          push(p2);
          transform(tab, false);
          p3 = pop();
          if (p3 === symbol(NIL)) {
            push_symbol(INTEGRAL);
            push(p1);
            push(p2);
            return list(3);
          } else {
            return push(p3);
          }
        };
        hashcode_values = {
          "x": 0.95532,
          "constexp": 1.43762,
          "constant": 1.1441659362941434,
          "constbase": 1.2036412230421882,
          "sin": 1.7330548251830322,
          "arcsin": 1.6483368529465805,
          "cos": 1.0586721236863401,
          "arccos": 1.8405225918106694,
          "tan": 1.1224943776292506,
          "arctan": 1.1297397925394963,
          "sinh": 1.8176164926060079,
          "cosh": 1.9404934661708022,
          "tanh": 1.6421307715103122,
          "log": 1.477443701354924,
          "erf": 1.0825269225702916
        };
        italu_hashcode = function(u4, x2) {
          var half;
          if (issymbol(u4)) {
            if (equal(u4, x2)) {
              return hashcode_values.x;
            } else {
              return hashcode_values.constant;
            }
          } else if (iscons(u4)) {
            switch (symnum(car(u4))) {
              case ADD:
                return hash_addition(cdr(u4), x2);
              case MULTIPLY:
                return hash_multiplication(cdr(u4), x2);
              case POWER:
                return hash_power(cadr(u4), caddr(u4), x2);
              case EXP:
                return hash_power(symbol(E), cadr(u4), x2);
              case SQRT:
                push_double(0.5);
                half = pop();
                return hash_power(cadr(u4), half, x2);
              default:
                return hash_function(u4, x2);
            }
          }
          return hashcode_values.constant;
        };
        hash_function = function(u4, x2) {
          var arg_hash, base, name;
          if (!Find(cadr(u4), x2)) {
            return hashcode_values.constant;
          }
          name = car(u4);
          arg_hash = italu_hashcode(cadr(u4), x2);
          base = hashcode_values[name.printname];
          if (!base) {
            throw new Error("Unsupported function " + name.printname);
          }
          return Math.pow(base, arg_hash);
        };
        hash_addition = function(terms, x2) {
          var k2, sum, term, term_hash, term_set, v2;
          term_set = {};
          while (iscons(terms)) {
            term = car(terms);
            terms = cdr(terms);
            term_hash = 0;
            if (Find(term, x2)) {
              term_hash = italu_hashcode(term, x2);
            } else {
              term_hash = hashcode_values.constant;
            }
            term_set[term_hash.toFixed(6)] = true;
          }
          sum = 0;
          for (k2 in term_set) {
            if (!hasProp.call(term_set, k2)) continue;
            v2 = term_set[k2];
            sum = sum + parseFloat(k2, 10);
          }
          return sum;
        };
        hash_multiplication = function(terms, x2) {
          var product, term;
          product = 1;
          while (iscons(terms)) {
            term = car(terms);
            terms = cdr(terms);
            if (Find(term, x2)) {
              product = product * italu_hashcode(term, x2);
            }
          }
          return product;
        };
        hash_power = function(base, power2, x2) {
          var base_hash, exp_hash;
          base_hash = hashcode_values.constant;
          exp_hash = hashcode_values.constexp;
          if (Find(base, x2)) {
            base_hash = italu_hashcode(base, x2);
          }
          if (Find(power2, x2)) {
            exp_hash = italu_hashcode(power2, x2);
          } else {
            if (base_hash === hashcode_values.constant) {
              return hashcode_values.constant;
            }
            if (isminusone(power2)) {
              exp_hash = -1;
            } else if (isoneovertwo(power2)) {
              exp_hash = 0.5;
            } else if (isminusoneovertwo(power2)) {
              exp_hash = -0.5;
            } else if (equalq(power2, 2, 1)) {
              exp_hash = 2;
            } else if (equalq(power2, -2, 1)) {
              exp_hash = -2;
            }
          }
          return Math.pow(base_hash, exp_hash);
        };
        make_hashed_itab = function() {
          var f7, h5, key, len, o12, s7, tab, u4;
          tab = {};
          for (o12 = 0, len = itab.length; o12 < len; o12++) {
            s7 = itab[o12];
            if (!s7) {
              break;
            }
            scan_meta(s7);
            f7 = pop();
            u4 = cadr(f7);
            h5 = italu_hashcode(u4, symbol(METAX));
            key = h5.toFixed(6);
            if (!tab[key]) {
              tab[key] = [];
            }
            tab[key].push(s7);
          }
          console.log("hashed_itab = " + JSON.stringify(tab, null, 2));
          return tab;
        };
        $.make_hashed_itab = make_hashed_itab;
        hashed_itab = {
          "1.144166": ["f(a,a*x)"],
          "1.046770": ["f(1/x,log(x))"],
          "0.936400": ["f(x^a,x^(a+1)/(a+1))"],
          "1.095727": ["f(x^(-2),-x^(-1))"],
          "1.023118": ["f(x^(-1/2),2*x^(1/2))"],
          "0.977405": ["f(x^(1/2),2/3*x^(3/2))"],
          "0.955320": ["f(x,x^2/2)"],
          "0.912636": ["f(x^2,x^3/3)"],
          "1.137302": ["f(exp(a*x),1/a*exp(a*x))", "f(a^x,a^x/log(a),or(not(number(a)),a>0))"],
          "1.326774": ["f(exp(a*x+b),1/a*exp(a*x+b))"],
          "1.080259": ["f(x*exp(a*x^2),exp(a*x^2)/(2*a))"],
          "1.260228": ["f(x*exp(a*x^2+b),exp(a*x^2+b)/(2*a))"],
          "1.451902": ["f(log(a*x),x*log(a*x)-x)"],
          "0.486192": ["f(1/(a+x^2),1/sqrt(a)*arctan(x/sqrt(a)),or(not(number(a)),a>0))", "f(1/(a-x^2),1/sqrt(a)*arctanh(x/sqrt(a)))", "f(1/(a+b*x^2),1/sqrt(a*b)*arctan(x*sqrt(a*b)/a),or(not(number(a*b)),a*b>0))", "f(1/(a+b*x^2),1/(2*sqrt(-a*b))*log((a+x*sqrt(-a*b))/(a-x*sqrt(-a*b))),or(not(number(a*b)),a*b<0))"],
          "0.697274": ["f(1/sqrt(a-x^2),arcsin(x/(sqrt(a))))", "f(1/sqrt(a+x^2),log(x+sqrt(a+x^2)))", "f(1/sqrt(x^2+a),log(x+sqrt(x^2+a)))"],
          "0.476307": ["f(1/(a+b*x),1/b*log(a+b*x))"],
          "0.226868": ["f(1/(a+b*x)^2,-1/(b*(a+b*x)))"],
          "2.904531": ["f(1/(a+b*x)^3,-1/(2*b)*1/(a+b*x)^2)"],
          "0.455026": ["f(x/(a+b*x),x/b-a*log(a+b*x)/b/b)"],
          "0.216732": ["f(x/(a+b*x)^2,1/b^2*(log(a+b*x)+a/(a+b*x)))"],
          "0.434695": ["f(x^2/(a+b*x),1/b^2*(1/2*(a+b*x)^2-2*a*(a+b*x)+a^2*log(a+b*x)))"],
          "0.207048": ["f(x^2/(a+b*x)^2,1/b^3*(a+b*x-2*a*log(a+b*x)-a^2/(a+b*x)))"],
          "2.650781": ["f(x^2/(a+b*x)^3,1/b^3*(log(a+b*x)+2*a/(a+b*x)-1/2*a^2/(a+b*x)^2))"],
          "0.498584": ["f(1/x*1/(a+b*x),-1/a*log((a+b*x)/x))"],
          "0.237479": ["f(1/x*1/(a+b*x)^2,1/a*1/(a+b*x)-1/a^2*log((a+b*x)/x))"],
          "3.040375": ["f(1/x*1/(a+b*x)^3,1/a^3*(1/2*((2*a+b*x)/(a+b*x))^2+log(x/(a+b*x))))"],
          "0.521902": ["f(1/x^2*1/(a+b*x),-1/(a*x)+b/a^2*log((a+b*x)/x))"],
          "0.446014": ["f(1/x^3*1/(a+b*x),(2*b*x-a)/(2*a^2*x^2)+b^2/a^3*log(x/(a+b*x)))"],
          "0.248586": ["f(1/x^2*1/(a+b*x)^2,-(a+2*b*x)/(a^2*x*(a+b*x))+2*b/a^3*log((a+b*x)/x))"],
          "0.464469": ["f(x/(a+b*x^2),1/2*1/b*log(a+b*x^2))"],
          "0.443716": ["f(x^2/(a+b*x^2),x/b-a/b*integral(1/(a+b*x^2),x))"],
          "0.236382": ["f(1/(a+b*x^2)^2,x/(2*a*(a+b*x^2))+1/2*1/a*integral(1/(a+b*x^2),x))"],
          "0.508931": ["f(1/x*1/(a+b*x^2),1/2*1/a*log(x^2/(a+b*x^2)))"],
          "0.532733": ["f(1/x^2*1/(a+b*x^2),-1/(a*x)-b/a*integral(1/(a+b*x^2),x))"],
          "0.480638": ["f(1/(a+b*x^3),1/3*1/a*(a/b)^(1/3)*(1/2*log(((a/b)^(1/3)+x)^3/(a+b*x^3))+sqrt(3)*arctan((2*x-(a/b)^(1/3))*(a/b)^(-1/3)/sqrt(3))))"],
          "0.438648": ["f(x^2/(a+b*x^3),1/3*1/b*log(a+b*x^3))"],
          "0.459164": ["f(x/(a+b*x^4),1/2*sqrt(b/a)/b*arctan(x^2*sqrt(b/a)),or(not(number(a*b)),a*b>0))", "f(x/(a+b*x^4),1/4*sqrt(-b/a)/b*log((x^2-sqrt(-a/b))/(x^2+sqrt(-a/b))),or(not(number(a*b)),a*b<0))"],
          "0.450070": ["f(x^3/(a+b*x^4),1/4*1/b*log(a+b*x^4))"],
          "1.448960": ["f(sqrt(a+b*x),2/3*1/b*sqrt((a+b*x)^3))"],
          "1.384221": ["f(x*sqrt(a+b*x),-2*(2*a-3*b*x)*sqrt((a+b*x)^3)/15/b^2)"],
          "1.322374": ["f(x^2*sqrt(a+b*x),2*(8*a^2-12*a*b*x+15*b^2*x^2)*sqrt((a+b*x)^3)/105/b^3)"],
          "1.516728": ["f(sqrt(a+b*x)/x,2*sqrt(a+b*x)+a*integral(1/x*1/sqrt(a+b*x),x))"],
          "1.587665": ["f(sqrt(a+b*x)/x^2,-sqrt(a+b*x)/x+b/2*integral(1/x*1/sqrt(a+b*x),x))"],
          "0.690150": ["f(1/sqrt(a+b*x),2*sqrt(a+b*x)/b)"],
          "0.659314": ["f(x/sqrt(a+b*x),-2/3*(2*a-b*x)*sqrt(a+b*x)/b^2)"],
          "0.629856": ["f(x^2/sqrt(a+b*x),2/15*(8*a^2-4*a*b*x+3*b^2*x^2)*sqrt(a+b*x)/b^3)"],
          "0.722428": ["f(1/x*1/sqrt(a+b*x),1/sqrt(a)*log((sqrt(a+b*x)-sqrt(a))/(sqrt(a+b*x)+sqrt(a))),or(not(number(a)),a>0))", "f(1/x*1/sqrt(a+b*x),2/sqrt(-a)*arctan(sqrt(-(a+b*x)/a)),or(not(number(a)),a<0))"],
          "0.756216": ["f(1/x^2*1/sqrt(a+b*x),-sqrt(a+b*x)/a/x-1/2*b/a*integral(1/x*1/sqrt(a+b*x),x))"],
          "1.434156": ["f(sqrt(x^2+a),1/2*(x*sqrt(x^2+a)+a*log(x+sqrt(x^2+a))))", "f(sqrt(a-x^2),1/2*(x*sqrt(a-x^2)+a*arcsin(x/sqrt(abs(a)))))", "f(sqrt(a*x^2+b),x*sqrt(a*x^2+b)/2+b*log(x*sqrt(a)+sqrt(a*x^2+b))/2/sqrt(a),and(number(a),a>0))", "f(sqrt(a*x^2+b),x*sqrt(a*x^2+b)/2+b*arcsin(x*sqrt(-a/b))/2/sqrt(-a),and(number(a),a<0))"],
          "0.729886": ["f(1/x*1/sqrt(x^2+a),arcsec(x/sqrt(-a))/sqrt(-a),or(not(number(a)),a<0))", "f(1/x*1/sqrt(x^2+a),-1/sqrt(a)*log((sqrt(a)+sqrt(x^2+a))/x),or(not(number(a)),a>0))", "f(1/x*1/sqrt(a-x^2),-1/sqrt(a)*log((sqrt(a)+sqrt(a-x^2))/x),or(not(number(a)),a>0))"],
          "1.501230": ["f(sqrt(x^2+a)/x,sqrt(x^2+a)-sqrt(a)*log((sqrt(a)+sqrt(x^2+a))/x),or(not(number(a)),a>0))", "f(sqrt(x^2+a)/x,sqrt(x^2+a)-sqrt(-a)*arcsec(x/sqrt(-a)),or(not(number(a)),a<0))", "f(sqrt(a-x^2)/x,sqrt(a-x^2)-sqrt(a)*log((sqrt(a)+sqrt(a-x^2))/x),or(not(number(a)),a>0))"],
          "0.666120": ["f(x/sqrt(x^2+a),sqrt(x^2+a))", "f(x/sqrt(a-x^2),-sqrt(a-x^2))"],
          "1.370077": ["f(x*sqrt(x^2+a),1/3*sqrt((x^2+a)^3))", "f(x*sqrt(a-x^2),-1/3*sqrt((a-x^2)^3))"],
          "1.730087": ["f(sqrt(a+x^6+3*a^(1/3)*x^4+3*a^(2/3)*x^2),1/4*(x*sqrt((x^2+a^(1/3))^3)+3/2*a^(1/3)*x*sqrt(x^2+a^(1/3))+3/2*a^(2/3)*log(x+sqrt(x^2+a^(1/3)))))", "f(sqrt(-a+x^6-3*a^(1/3)*x^4+3*a^(2/3)*x^2),1/4*(x*sqrt((x^2-a^(1/3))^3)-3/2*a^(1/3)*x*sqrt(x^2-a^(1/3))+3/2*a^(2/3)*log(x+sqrt(x^2-a^(1/3)))))"],
          "0.578006": ["f(1/sqrt(a+x^6+3*a^(1/3)*x^4+3*a^(2/3)*x^2),x/a^(1/3)/sqrt(x^2+a^(1/3)))"],
          "0.552180": ["f(x/sqrt(a+x^6+3*a^(1/3)*x^4+3*a^(2/3)*x^2),-1/sqrt(x^2+a^(1/3)))"],
          "1.652787": ["f(x*sqrt(a+x^6+3*a^(1/3)*x^4+3*a^(2/3)*x^2),1/5*sqrt((x^2+a^(1/3))^5))"],
          "1.308862": ["f(x^2*sqrt(x^2+a),1/4*x*sqrt((x^2+a)^3)-1/8*a*x*sqrt(x^2+a)-1/8*a^2*log(x+sqrt(x^2+a)))", "f(x^2*sqrt(a-x^2),-x/4*sqrt((a-x^2)^3)+1/8*a*(x*sqrt(a-x^2)+a*arcsin(x/sqrt(a))),or(not(number(a)),a>0))"],
          "1.342944": ["f(x^3*sqrt(x^2+a),(1/5*x^2-2/15*a)*sqrt((x^2+a)^3),and(number(a),a>0))", "f(x^3*sqrt(x^2+a),sqrt((x^2+a)^5)/5-a*sqrt((x^2+a)^3)/3,and(number(a),a<0))", "f(x^3*sqrt(a-x^2),(-1/5*x^2-2/15*a)*sqrt((a-x^2)^3),or(not(number(a)),a>0))", "f(sqrt(a-x^2)/x^3,-1/2*sqrt(a-x^2)/x^2+1/2*log((sqrt(a)+sqrt(a-x^2))/x)/sqrt(a),or(not(number(a)),a>0))", "f(sqrt(a-x^2)/x^4,-1/3*sqrt((a-x^2)^3)/a/x^3,or(not(number(a)),a>0))"],
          "0.636358": ["f(x^2/sqrt(x^2+a),1/2*x*sqrt(x^2+a)-1/2*a*log(x+sqrt(x^2+a)))", "f(x^2/sqrt(a-x^2),-x/2*sqrt(a-x^2)+a/2*arcsin(x/sqrt(a)),or(not(number(a)),a>0))"],
          "0.652928": ["f(x^3/sqrt(x^2+a),1/3*sqrt((x^2+a)^3)-a*sqrt(x^2+a))", "f(1/x^3*1/sqrt(x^2+a),-1/2*sqrt(x^2+a)/a/x^2+1/2*log((sqrt(a)+sqrt(x^2+a))/x)/a^(3/2),or(not(number(a)),a>0))", "f(1/x^3*1/sqrt(x^2-a),1/2*sqrt(x^2-a)/a/x^2+1/2*1/(a^(3/2))*arcsec(x/(a^(1/2))),or(not(number(a)),a>0))"],
          "0.764022": ["f(1/x^2*1/sqrt(x^2+a),-sqrt(x^2+a)/a/x)", "f(1/x^2*1/sqrt(a-x^2),-sqrt(a-x^2)/a/x,or(not(number(a)),a>0))"],
          "1.578940": ["f(x^2*sqrt(a+x^6+3*a^(1/3)*x^4+3*a^(2/3)*x^2),1/6*x*sqrt((x^2+a^(1/3))^5)-1/24*a^(1/3)*x*sqrt((x^2+a^(1/3))^3)-1/16*a^(2/3)*x*sqrt(x^2+a^(1/3))-1/16*a*log(x+sqrt(x^2+a^(1/3))),or(not(number(a)),a>0))", "f(x^2*sqrt(-a-3*a^(1/3)*x^4+3*a^(2/3)*x^2+x^6),1/6*x*sqrt((x^2-a^(1/3))^5)+1/24*a^(1/3)*x*sqrt((x^2-a^(1/3))^3)-1/16*a^(2/3)*x*sqrt(x^2-a^(1/3))+1/16*a*log(x+sqrt(x^2-a^(1/3))),or(not(number(a)),a>0))"],
          "1.620055": ["f(x^3*sqrt(a+x^6+3*a^(1/3)*x^4+3*a^(2/3)*x^2),1/7*sqrt((x^2+a^(1/3))^7)-1/5*a^(1/3)*sqrt((x^2+a^(1/3))^5),or(not(number(a)),a>0))", "f(x^3*sqrt(-a-3*a^(1/3)*x^4+3*a^(2/3)*x^2+x^6),1/7*sqrt((x^2-a^(1/3))^7)+1/5*a^(1/3)*sqrt((x^2-a^(1/3))^5),or(not(number(a)),a>0))"],
          "0.332117": ["f(1/(x-a)/sqrt(x^2-a^2),-sqrt(x^2-a^2)/a/(x-a))", "f(1/(x+a)/sqrt(x^2-a^2),sqrt(x^2-a^2)/a/(x+a))"],
          "1.571443": ["f(sqrt(a-x^2)/x^2,-sqrt(a-x^2)/x-arcsin(x/sqrt(a)),or(not(number(a)),a>0))"],
          "1.690994": ["f(sin(a*x),-cos(a*x)/a)"],
          "1.055979": ["f(cos(a*x),sin(a*x)/a)"],
          "1.116714": ["f(tan(a*x),-log(cos(a*x))/a)"],
          "0.895484": ["f(1/tan(a*x),log(sin(a*x))/a)"],
          "0.946989": ["f(1/cos(a*x),log(tan(pi/4+a*x/2))/a)"],
          "0.591368": ["f(1/sin(a*x),log(tan(a*x/2))/a)"],
          "2.859462": ["f(sin(a*x)^2,x/2-sin(2*a*x)/(4*a))"],
          "2.128050": ["f(sin(a*x)^3,-cos(a*x)*(sin(a*x)^2+2)/(3*a))", "f(sin(a*x)^4,3/8*x-sin(2*a*x)/(4*a)+sin(4*a*x)/(32*a))"],
          "1.115091": ["f(cos(a*x)^2,x/2+sin(2*a*x)/(4*a))"],
          "1.081452": ["f(cos(a*x)^3,sin(a*x)*(cos(a*x)^2+2)/(3*a))", "f(cos(a*x)^4,3/8*x+sin(2*a*x)/(4*a)+sin(4*a*x)/(32*a))"],
          "0.349716": ["f(1/sin(a*x)^2,-1/(a*tan(a*x)))"],
          "0.896788": ["f(1/cos(a*x)^2,tan(a*x)/a)"],
          "1.785654": ["f(sin(a*x)*cos(a*x),sin(a*x)^2/(2*a))"],
          "3.188560": ["f(sin(a*x)^2*cos(a*x)^2,-sin(4*a*x)/(32*a)+x/8)"],
          "1.516463": ["f(sin(a*x)/cos(a*x)^2,1/(a*cos(a*x)))"],
          "2.707879": ["f(sin(a*x)^2/cos(a*x),(log(tan(pi/4+a*x/2))-sin(a*x))/a)"],
          "0.369293": ["f(cos(a*x)/sin(a*x)^2,-1/(a*sin(a*x)))"],
          "0.560019": ["f(1/(sin(a*x)*cos(a*x)),log(tan(a*x))/a)"],
          "0.530332": ["f(1/(sin(a*x)*cos(a*x)^2),(1/cos(a*x)+log(tan(a*x/2)))/a)"],
          "0.331177": ["f(1/(sin(a*x)^2*cos(a*x)),(log(tan(pi/4+a*x/2))-1/sin(a*x))/a)"],
          "0.313621": ["f(1/(sin(a*x)^2*cos(a*x)^2),-2/(a*tan(2*a*x)))"],
          "3.172365": ["f(sin(a+b*x),-cos(a+b*x)/b)"],
          "1.127162": ["f(cos(a+b*x),sin(a+b*x)/b)"],
          "0.352714": ["f(1/(b+b*sin(a*x)),-tan(pi/4-a*x/2)/a/b)", "f(1/(b-b*sin(a*x)),tan(pi/4+a*x/2)/a/b)", "f(1/(a+b*sin(x)),1/sqrt(b^2-a^2)*log((a*tan(x/2)+b-sqrt(b^2-a^2))/(a*tan(x/2)+b+sqrt(b^2-a^2))),b^2-a^2)"],
          "0.454515": ["f(1/(b+b*cos(a*x)),tan(a*x/2)/a/b)", "f(1/(b-b*cos(a*x)),-1/tan(a*x/2)/a/b)", "f(1/(a+b*cos(x)),1/sqrt(b^2-a^2)*log((sqrt(b^2-a^2)*tan(x/2)+a+b)/(sqrt(b^2-a^2)*tan(x/2)-a-b)),b^2-a^2)"],
          "1.615441": ["f(x*sin(a*x),sin(a*x)/a^2-x*cos(a*x)/a)"],
          "1.543263": ["f(x^2*sin(a*x),2*x*sin(a*x)/a^2-(a^2*x^2-2)*cos(a*x)/a^3)"],
          "1.008798": ["f(x*cos(a*x),cos(a*x)/a^2+x*sin(a*x)/a)"],
          "0.963724": ["f(x^2*cos(a*x),2*x*cos(a*x)/a^2+(a^2*x^2-2)*sin(a*x)/a^3)"],
          "1.611938": ["f(arcsin(a*x),x*arcsin(a*x)+sqrt(1-a^2*x^2)/a)"],
          "1.791033": ["f(arccos(a*x),x*arccos(a*x)-sqrt(1-a^2*x^2)/a)"],
          "1.123599": ["f(arctan(a*x),x*arctan(a*x)-1/2*log(1+a^2*x^2)/a)"],
          "1.387031": ["f(x*log(a*x),x^2*log(a*x)/2-x^2/4)"],
          "1.325058": ["f(x^2*log(a*x),x^3*log(a*x)/3-1/9*x^3)"],
          "2.108018": ["f(log(x)^2,x*log(x)^2-2*x*log(x)+2*x)"],
          "0.403214": ["f(1/x*1/(a+log(x)),log(a+log(x)))"],
          "2.269268": ["f(log(a*x+b),(a*x+b)*log(a*x+b)/a-x)"],
          "2.486498": ["f(log(a*x+b)/x^2,a/b*log(x)-(a*x+b)*log(a*x+b)/b/x)"],
          "1.769733": ["f(sinh(x),cosh(x))"],
          "1.883858": ["f(cosh(x),sinh(x))"],
          "1.606140": ["f(tanh(x),log(cosh(x)))"],
          "1.690661": ["f(x*sinh(x),x*cosh(x)-sinh(x))"],
          "1.799688": ["f(x*cosh(x),x*sinh(x)-cosh(x))"],
          "3.131954": ["f(sinh(x)^2,sinh(2*x)/4-x/2)"],
          "2.579685": ["f(tanh(x)^2,x-tanh(x))"],
          "3.548923": ["f(cosh(x)^2,sinh(2*x)/4+x/2)"],
          "1.058866": ["f(x^3*exp(a*x^2),exp(a*x^2)*(x^2/a-1/(a^2))/2)"],
          "1.235270": ["f(x^3*exp(a*x^2+b),exp(a*x^2)*exp(b)*(x^2/a-1/(a^2))/2)"],
          "1.130783": ["f(exp(a*x^2),-i*sqrt(pi)*erf(i*sqrt(a)*x)/sqrt(a)/2)"],
          "1.078698": ["f(erf(a*x),x*erf(a*x)+exp(-a^2*x^2)/a/sqrt(pi))"],
          "2.573650": ["f(x^2*(1-x^2)^(3/2),(x*sqrt(1-x^2)*(-8*x^4+14*x^2-3)+3*arcsin(x))/48)", "f(x^2*(1-x^2)^(5/2),(x*sqrt(1-x^2)*(48*x^6-136*x^4+118*x^2-15)+15*arcsin(x))/384)"],
          "2.640666": ["f(x^4*(1-x^2)^(3/2),(-x*sqrt(1-x^2)*(16*x^6-24*x^4+2*x^2+3)+3*arcsin(x))/128)"],
          "1.086487": ["f(x*exp(a*x),exp(a*x)*(a*x-1)/(a^2))"],
          "1.267493": ["f(x*exp(a*x+b),exp(a*x+b)*(a*x-1)/(a^2))"],
          "1.037943": ["f(x^2*exp(a*x),exp(a*x)*(a^2*x^2-2*a*x+2)/(a^3))"],
          "1.210862": ["f(x^2*exp(a*x+b),exp(a*x+b)*(a^2*x^2-2*a*x+2)/(a^3))"],
          "1.064970": ["f(x^3*exp(a*x),exp(a*x)*x^3/a-3/a*integral(x^2*exp(a*x),x))"],
          "1.242392": ["f(x^3*exp(a*x+b),exp(a*x+b)*x^3/a-3/a*integral(x^2*exp(a*x+b),x))"]
        };
        INV_check_arg = function() {
          if (!istensor(p1)) {
            return 0;
          } else if (p1.tensor.ndim !== 2) {
            return 0;
          } else if (p1.tensor.dim[0] !== p1.tensor.dim[1]) {
            return 0;
          } else {
            return 1;
          }
        };
        inv = function() {
          var accumulator, eachEntry, i5, n9, o12, ref2;
          i5 = 0;
          n9 = 0;
          save();
          p1 = pop();
          if (isinv(p1)) {
            push(car(cdr(p1)));
            restore();
            return;
          }
          if (isidentitymatrix(p1)) {
            push(p1);
            restore();
            return;
          }
          if (expanding && isinnerordot(p1)) {
            p1 = cdr(p1);
            accumulator = [];
            while (iscons(p1)) {
              accumulator.push(car(p1));
              p1 = cdr(p1);
            }
            for (eachEntry = o12 = ref2 = accumulator.length - 1; ref2 <= 0 ? o12 <= 0 : o12 >= 0; eachEntry = ref2 <= 0 ? ++o12 : --o12) {
              push(accumulator[eachEntry]);
              inv();
              if (eachEntry !== accumulator.length - 1) {
                inner();
              }
            }
            restore();
            return;
          }
          if (INV_check_arg() === 0) {
            push_symbol(INV);
            push(p1);
            list(2);
            restore();
            return;
          }
          if (isNumericAtomOrTensor(p1)) {
            yyinvg();
          } else {
            push(p1);
            adj();
            push(p1);
            det();
            p2 = pop();
            if (isZeroAtomOrTensor(p2)) {
              stop("inverse of singular matrix");
            }
            push(p2);
            divide();
          }
          return restore();
        };
        invg = function() {
          save();
          p1 = pop();
          if (INV_check_arg() === 0) {
            push_symbol(INVG);
            push(p1);
            list(2);
            restore();
            return;
          }
          yyinvg();
          return restore();
        };
        yyinvg = function() {
          var h5, i5, i12, j2, j12, l1, n9, o12, ref2, ref12, ref22, ref3;
          h5 = 0;
          i5 = 0;
          j2 = 0;
          n9 = 0;
          n9 = p1.tensor.dim[0];
          h5 = tos;
          for (i5 = o12 = 0, ref2 = n9; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            for (j2 = i12 = 0, ref12 = n9; 0 <= ref12 ? i12 < ref12 : i12 > ref12; j2 = 0 <= ref12 ? ++i12 : --i12) {
              if (i5 === j2) {
                push(one);
              } else {
                push(zero);
              }
            }
          }
          for (i5 = j12 = 0, ref22 = n9 * n9; 0 <= ref22 ? j12 < ref22 : j12 > ref22; i5 = 0 <= ref22 ? ++j12 : --j12) {
            push(p1.tensor.elem[i5]);
          }
          INV_decomp(n9);
          p1 = alloc_tensor(n9 * n9);
          p1.tensor.ndim = 2;
          p1.tensor.dim[0] = n9;
          p1.tensor.dim[1] = n9;
          for (i5 = l1 = 0, ref3 = n9 * n9; 0 <= ref3 ? l1 < ref3 : l1 > ref3; i5 = 0 <= ref3 ? ++l1 : --l1) {
            p1.tensor.elem[i5] = stack[h5 + i5];
          }
          moveTos(tos - 2 * n9 * n9);
          return push(p1);
        };
        INV_decomp = function(n9) {
          var a4, d3, i5, i12, j2, j12, l1, o12, ref2, ref12, ref22, ref3, ref4, results, u4;
          a4 = 0;
          d3 = 0;
          i5 = 0;
          j2 = 0;
          u4 = 0;
          a4 = tos - n9 * n9;
          u4 = a4 - n9 * n9;
          results = [];
          for (d3 = o12 = 0, ref2 = n9; 0 <= ref2 ? o12 < ref2 : o12 > ref2; d3 = 0 <= ref2 ? ++o12 : --o12) {
            if (equal(stack[a4 + n9 * d3 + d3], zero)) {
              for (i5 = i12 = ref12 = d3 + 1, ref22 = n9; ref12 <= ref22 ? i12 < ref22 : i12 > ref22; i5 = ref12 <= ref22 ? ++i12 : --i12) {
                if (!equal(stack[a4 + n9 * i5 + d3], zero)) {
                  break;
                }
              }
              if (i5 === n9) {
                stop("inverse of singular matrix");
              }
              for (j2 = j12 = 0, ref3 = n9; 0 <= ref3 ? j12 < ref3 : j12 > ref3; j2 = 0 <= ref3 ? ++j12 : --j12) {
                p2 = stack[a4 + n9 * d3 + j2];
                stack[a4 + n9 * d3 + j2] = stack[a4 + n9 * i5 + j2];
                stack[a4 + n9 * i5 + j2] = p2;
                p2 = stack[u4 + n9 * d3 + j2];
                stack[u4 + n9 * d3 + j2] = stack[u4 + n9 * i5 + j2];
                stack[u4 + n9 * i5 + j2] = p2;
              }
            }
            p2 = stack[a4 + n9 * d3 + d3];
            for (j2 = l1 = 0, ref4 = n9; 0 <= ref4 ? l1 < ref4 : l1 > ref4; j2 = 0 <= ref4 ? ++l1 : --l1) {
              if (j2 > d3) {
                push(stack[a4 + n9 * d3 + j2]);
                push(p2);
                divide();
                stack[a4 + n9 * d3 + j2] = pop();
              }
              push(stack[u4 + n9 * d3 + j2]);
              push(p2);
              divide();
              stack[u4 + n9 * d3 + j2] = pop();
            }
            results.push(function() {
              var m1, ref5, results1;
              results1 = [];
              for (i5 = m1 = 0, ref5 = n9; 0 <= ref5 ? m1 < ref5 : m1 > ref5; i5 = 0 <= ref5 ? ++m1 : --m1) {
                if (i5 === d3) {
                  continue;
                }
                p2 = stack[a4 + n9 * i5 + d3];
                results1.push(function() {
                  var n1, ref6, results2;
                  results2 = [];
                  for (j2 = n1 = 0, ref6 = n9; 0 <= ref6 ? n1 < ref6 : n1 > ref6; j2 = 0 <= ref6 ? ++n1 : --n1) {
                    if (j2 > d3) {
                      push(stack[a4 + n9 * i5 + j2]);
                      push(stack[a4 + n9 * d3 + j2]);
                      push(p2);
                      multiply();
                      subtract();
                      stack[a4 + n9 * i5 + j2] = pop();
                    }
                    push(stack[u4 + n9 * i5 + j2]);
                    push(stack[u4 + n9 * d3 + j2]);
                    push(p2);
                    multiply();
                    subtract();
                    results2.push(stack[u4 + n9 * i5 + j2] = pop());
                  }
                  return results2;
                }());
              }
              return results1;
            }());
          }
          return results;
        };
        DEBUG_IS = false;
        isZeroAtom = function(p11) {
          switch (p11.k) {
            case NUM:
              return MZERO(p11.q.a);
            case DOUBLE:
              return p11.d === 0;
            default:
              return false;
          }
        };
        isZeroTensor = function(p11) {
          var i5, o12, ref2;
          if (p11.k !== TENSOR) {
            return 0;
          }
          for (i5 = o12 = 0, ref2 = p11.tensor.nelem; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            if (!isZeroAtomOrTensor(p11.tensor.elem[i5])) {
              return 0;
            }
          }
          return 1;
        };
        isZeroAtomOrTensor = function(p11) {
          return isZeroAtom(p11) || isZeroTensor(p11);
        };
        isZeroLikeOrNonZeroLikeOrUndetermined = function(valueOrPredicate) {
          var evalledArgument;
          push(valueOrPredicate);
          Eval_predicate();
          evalledArgument = pop();
          if (isZeroAtomOrTensor(evalledArgument)) {
            return 0;
          }
          if (isNumericAtomOrTensor(evalledArgument)) {
            return 1;
          }
          push(evalledArgument);
          zzfloat();
          evalledArgument = pop();
          if (isZeroAtomOrTensor(evalledArgument)) {
            return 0;
          }
          if (isNumericAtomOrTensor(evalledArgument)) {
            return 1;
          }
          if (Find(evalledArgument, imaginaryunit)) {
            push(evalledArgument);
            absValFloat();
            Eval_predicate();
            evalledArgument = pop();
            if (isZeroAtomOrTensor(evalledArgument)) {
              return 0;
            }
            if (isNumericAtomOrTensor(evalledArgument)) {
              return 1;
            }
          }
          return null;
        };
        isnegativenumber = function(p11) {
          switch (p11.k) {
            case NUM:
              if (MSIGN(p11.q.a) === -1) {
                return 1;
              }
              break;
            case DOUBLE:
              if (p11.d < 0) {
                return 1;
              }
          }
          return 0;
        };
        ispositivenumber = function(p11) {
          switch (p11.k) {
            case NUM:
              if (MSIGN(p11.q.a) === 1) {
                return 1;
              }
              break;
            case DOUBLE:
              if (p11.d > 0) {
                return 1;
              }
          }
          return 0;
        };
        isplustwo = function(p11) {
          switch (p11.k) {
            case NUM:
              if (MEQUAL(p11.q.a, 2) && MEQUAL(p11.q.b, 1)) {
                return 1;
              }
              break;
            case DOUBLE:
              if (p11.d === 2) {
                return 1;
              }
          }
          return 0;
        };
        isplusone = function(p11) {
          switch (p11.k) {
            case NUM:
              if (MEQUAL(p11.q.a, 1) && MEQUAL(p11.q.b, 1)) {
                return 1;
              }
              break;
            case DOUBLE:
              if (p11.d === 1) {
                return 1;
              }
          }
          return 0;
        };
        isminusone = function(p11) {
          switch (p11.k) {
            case NUM:
              if (MEQUAL(p11.q.a, -1) && MEQUAL(p11.q.b, 1)) {
                return 1;
              }
              break;
            case DOUBLE:
              if (p11.d === -1) {
                return 1;
              }
          }
          return 0;
        };
        isone = function(p11) {
          return isplusone(p11) || isminusone(p11);
        };
        isinteger = function(p11) {
          if (p11.k === NUM && MEQUAL(p11.q.b, 1)) {
            return 1;
          } else {
            return 0;
          }
        };
        isintegerorintegerfloat = function(p11) {
          if (p11.k === DOUBLE) {
            if (p11.d === Math.round(p11.d)) {
              return 1;
            }
            return 0;
          }
          return isinteger(p11);
        };
        isnonnegativeinteger = function(p11) {
          if (isrational(p11) && MEQUAL(p11.q.b, 1) && MSIGN(p11.q.a) === 1) {
            return 1;
          } else {
            return 0;
          }
        };
        isposint = function(p11) {
          if (isinteger(p11) && MSIGN(p11.q.a) === 1) {
            return 1;
          } else {
            return 0;
          }
        };
        isunivarpolyfactoredorexpandedform = function(p11, x2) {
          if (DEBUG) {
            console.log("isunivarpolyfactoredorexpandedform: p: " + p11 + " x: " + x2);
          }
          if (x2 == null) {
            push(p11);
            guess();
            x2 = pop();
            pop();
          }
          if (ispolyfactoredorexpandedform(p11, x2) && Find(p11, symbol(SYMBOL_X)) + Find(p11, symbol(SYMBOL_Y)) + Find(p11, symbol(SYMBOL_Z)) === 1) {
            return x2;
          } else {
            return 0;
          }
        };
        ispolyfactoredorexpandedform = function(p11, x2) {
          return ispolyfactoredorexpandedform_factor(p11, x2);
        };
        ispolyfactoredorexpandedform_factor = function(p11, x2) {
          if (car(p11) === symbol(MULTIPLY)) {
            p11 = cdr(p11);
            while (iscons(p11)) {
              if (DEBUG) {
                console.log("ispolyfactoredorexpandedform_factor testing " + car(p11));
              }
              if (!ispolyfactoredorexpandedform_power(car(p11), x2)) {
                if (DEBUG) {
                  console.log("... tested negative:" + car(p11));
                }
                return 0;
              }
              p11 = cdr(p11);
            }
            return 1;
          } else {
            return ispolyfactoredorexpandedform_power(p11, x2);
          }
        };
        ispolyfactoredorexpandedform_power = function(p11, x2) {
          if (car(p11) === symbol(POWER)) {
            if (DEBUG) {
              console.log("ispolyfactoredorexpandedform_power (isposint(caddr(p)) " + (isposint(caddr(p11)), DEBUG ? console.log("ispolyfactoredorexpandedform_power ispolyexpandedform_expr(cadr(p), x)) " + ispolyexpandedform_expr(cadr(p11), x2)) : void 0));
            }
            return isposint(caddr(p11)) && ispolyexpandedform_expr(cadr(p11), x2);
          } else {
            if (DEBUG) {
              console.log("ispolyfactoredorexpandedform_power not a power, testing if this is exp form: " + p11);
            }
            return ispolyexpandedform_expr(p11, x2);
          }
        };
        ispolyexpandedform = function(p11, x2) {
          if (Find(p11, x2)) {
            return ispolyexpandedform_expr(p11, x2);
          } else {
            return 0;
          }
        };
        ispolyexpandedform_expr = function(p11, x2) {
          if (car(p11) === symbol(ADD)) {
            p11 = cdr(p11);
            while (iscons(p11)) {
              if (!ispolyexpandedform_term(car(p11), x2)) {
                return 0;
              }
              p11 = cdr(p11);
            }
            return 1;
          } else {
            return ispolyexpandedform_term(p11, x2);
          }
        };
        ispolyexpandedform_term = function(p11, x2) {
          if (car(p11) === symbol(MULTIPLY)) {
            p11 = cdr(p11);
            while (iscons(p11)) {
              if (!ispolyexpandedform_factor(car(p11), x2)) {
                return 0;
              }
              p11 = cdr(p11);
            }
            return 1;
          } else {
            return ispolyexpandedform_factor(p11, x2);
          }
        };
        ispolyexpandedform_factor = function(p11, x2) {
          if (equal(p11, x2)) {
            return 1;
          }
          if (car(p11) === symbol(POWER) && equal(cadr(p11), x2)) {
            if (isposint(caddr(p11))) {
              return 1;
            } else {
              return 0;
            }
          }
          if (Find(p11, x2)) {
            return 0;
          } else {
            return 1;
          }
        };
        isnegativeterm = function(p11) {
          if (isnegativenumber(p11)) {
            return 1;
          } else if (car(p11) === symbol(MULTIPLY) && isnegativenumber(cadr(p11))) {
            return 1;
          } else {
            return 0;
          }
        };
        hasNegativeRationalExponent = function(p11) {
          if (car(p11) === symbol(POWER) && isrational(car(cdr(cdr(p11)))) && isnegativenumber(car(cdr(p11)))) {
            if (DEBUG_IS) {
              console.log("hasNegativeRationalExponent: " + p11.toString() + " has imaginary component");
            }
            return 1;
          } else {
            if (DEBUG_IS) {
              console.log("hasNegativeRationalExponent: " + p11.toString() + " has NO imaginary component");
            }
            return 0;
          }
        };
        isimaginarynumberdouble = function(p11) {
          if (car(p11) === symbol(MULTIPLY) && length(p11) === 3 && isdouble(cadr(p11)) && hasNegativeRationalExponent(caddr(p11)) || equal(p11, imaginaryunit)) {
            return 1;
          } else {
            return 0;
          }
        };
        isimaginarynumber = function(p11) {
          if (car(p11) === symbol(MULTIPLY) && length(p11) === 3 && isNumericAtom(cadr(p11)) && equal(caddr(p11), imaginaryunit) || equal(p11, imaginaryunit) || hasNegativeRationalExponent(caddr(p11))) {
            if (DEBUG_IS) {
              console.log("isimaginarynumber: " + p11.toString() + " is imaginary number");
            }
            return 1;
          } else {
            if (DEBUG_IS) {
              console.log("isimaginarynumber: " + p11.toString() + " isn't an imaginary number");
            }
            return 0;
          }
        };
        iscomplexnumberdouble = function(p11) {
          if (car(p11) === symbol(ADD) && length(p11) === 3 && isdouble(cadr(p11)) && isimaginarynumberdouble(caddr(p11)) || isimaginarynumberdouble(p11)) {
            return 1;
          } else {
            return 0;
          }
        };
        iscomplexnumber = function(p11) {
          if (DEBUG_IS) {
            debugger;
          }
          if (car(p11) === symbol(ADD) && length(p11) === 3 && isNumericAtom(cadr(p11)) && isimaginarynumber(caddr(p11)) || isimaginarynumber(p11)) {
            if (DEBUG) {
              console.log("iscomplexnumber: " + p11.toString() + " is imaginary number");
            }
            return 1;
          } else {
            if (DEBUG) {
              console.log("iscomplexnumber: " + p11.toString() + " is imaginary number");
            }
            return 0;
          }
        };
        iseveninteger = function(p11) {
          if (isinteger(p11) && p11.q.a.isEven()) {
            return 1;
          } else {
            return 0;
          }
        };
        isnegative = function(p11) {
          if (car(p11) === symbol(ADD) && isnegativeterm(cadr(p11))) {
            return 1;
          } else if (isnegativeterm(p11)) {
            return 1;
          } else {
            return 0;
          }
        };
        issymbolic = function(p11) {
          if (issymbol(p11)) {
            return 1;
          } else {
            while (iscons(p11)) {
              if (issymbolic(car(p11))) {
                return 1;
              }
              p11 = cdr(p11);
            }
            return 0;
          }
        };
        isintegerfactor = function(p11) {
          return isinteger(p11) || car(p11) === symbol(POWER) && isinteger(cadr(p11)) && isinteger(caddr(p11));
        };
        isNumberOneOverSomething = function(p11) {
          return isfraction(p11) && MEQUAL(p11.q.a.abs(), 1);
        };
        isoneover = function(p11) {
          return car(p11) === symbol(POWER) && isminusone(caddr(p11));
        };
        isfraction = function(p11) {
          return p11.k === NUM && !MEQUAL(p11.q.b, 1);
        };
        equaln = function(p11, n9) {
          switch (p11.k) {
            case NUM:
              return MEQUAL(p11.q.a, n9) && MEQUAL(p11.q.b, 1);
            case DOUBLE:
              return p11.d === n9;
            default:
              return false;
          }
        };
        equalq = function(p11, a4, b2) {
          switch (p11.k) {
            case NUM:
              return MEQUAL(p11.q.a, a4) && MEQUAL(p11.q.b, b2);
            case DOUBLE:
              return p11.d === a4 / b2;
            default:
              return false;
          }
        };
        isoneovertwo = function(p11) {
          return equalq(p11, 1, 2);
        };
        isminusoneovertwo = function(p11) {
          return equalq(p11, -1, 2);
        };
        isoneoversqrttwo = function(p11) {
          return car(p11) === symbol(POWER) && equaln(cadr(p11), 2) && equalq(caddr(p11), -1, 2);
        };
        isminusoneoversqrttwo = function(p11) {
          return car(p11) === symbol(MULTIPLY) && equaln(cadr(p11), -1) && isoneoversqrttwo(caddr(p11)) && length(p11) === 3;
        };
        issqrtthreeovertwo = function(p11) {
          return car(p11) === symbol(MULTIPLY) && isoneovertwo(cadr(p11)) && issqrtthree(caddr(p11)) && length(p11) === 3;
        };
        isminussqrtthreeovertwo = function(p11) {
          return car(p11) === symbol(MULTIPLY) && isminusoneovertwo(cadr(p11)) && issqrtthree(caddr(p11)) && length(p11) === 3;
        };
        issqrtthree = function(p11) {
          return car(p11) === symbol(POWER) && equaln(cadr(p11), 3) && isoneovertwo(caddr(p11));
        };
        isfloating = function(p11) {
          if (p11.k === DOUBLE || p11 === symbol(FLOATF)) {
            return 1;
          }
          while (iscons(p11)) {
            if (isfloating(car(p11))) {
              return 1;
            }
            p11 = cdr(p11);
          }
          return 0;
        };
        isimaginaryunit = function(p11) {
          if (equal(p11, imaginaryunit)) {
            return 1;
          } else {
            return 0;
          }
        };
        isquarterturn = function(p11) {
          var minussign, n9;
          n9 = 0;
          minussign = 0;
          if (car(p11) !== symbol(MULTIPLY)) {
            return 0;
          }
          if (equal(cadr(p11), imaginaryunit)) {
            if (caddr(p11) !== symbol(PI)) {
              return 0;
            }
            if (length(p11) !== 3) {
              return 0;
            }
            return 2;
          }
          if (!isNumericAtom(cadr(p11))) {
            return 0;
          }
          if (!equal(caddr(p11), imaginaryunit)) {
            return 0;
          }
          if (cadddr(p11) !== symbol(PI)) {
            return 0;
          }
          if (length(p11) !== 4) {
            return 0;
          }
          push(cadr(p11));
          push_integer(2);
          multiply();
          n9 = pop_integer();
          if (isNaN(n9)) {
            return 0;
          }
          if (n9 < 1) {
            minussign = 1;
            n9 = -n9;
          }
          switch (n9 % 4) {
            case 0:
              n9 = 1;
              break;
            case 1:
              if (minussign) {
                n9 = 4;
              } else {
                n9 = 3;
              }
              break;
            case 2:
              n9 = 2;
              break;
            case 3:
              if (minussign) {
                n9 = 3;
              } else {
                n9 = 4;
              }
          }
          return n9;
        };
        isnpi = function(p11) {
          var doNothing, n9;
          n9 = 0;
          if (p11 === symbol(PI)) {
            return 2;
          }
          if (car(p11) === symbol(MULTIPLY) && isNumericAtom(cadr(p11)) && caddr(p11) === symbol(PI) && length(p11) === 3) {
            doNothing = 0;
          } else {
            return 0;
          }
          push(cadr(p11));
          push_integer(2);
          multiply();
          n9 = pop_integer();
          if (isNaN(n9)) {
            return 0;
          }
          if (n9 < 0) {
            n9 = 4 - -n9 % 4;
          } else {
            n9 = 1 + (n9 - 1) % 4;
          }
          return n9;
        };
        $.isZeroAtomOrTensor = isZeroAtomOrTensor;
        $.isnegativenumber = isnegativenumber;
        $.isplusone = isplusone;
        $.isminusone = isminusone;
        $.isinteger = isinteger;
        $.isnonnegativeinteger = isnonnegativeinteger;
        $.isposint = isposint;
        $.isnegativeterm = isnegativeterm;
        $.isimaginarynumber = isimaginarynumber;
        $.iscomplexnumber = iscomplexnumber;
        $.iseveninteger = iseveninteger;
        $.isnegative = isnegative;
        $.issymbolic = issymbolic;
        $.isintegerfactor = isintegerfactor;
        $.isoneover = isoneover;
        $.isfraction = isfraction;
        $.isoneoversqrttwo = isoneoversqrttwo;
        $.isminusoneoversqrttwo = isminusoneoversqrttwo;
        $.isfloating = isfloating;
        $.isimaginaryunit = isimaginaryunit;
        $.isquarterturn = isquarterturn;
        $.isnpi = isnpi;
        Eval_isprime = function() {
          push(cadr(p1));
          Eval();
          p1 = pop();
          if (isnonnegativeinteger(p1) && mprime(p1.q.a)) {
            return push_integer(1);
          } else {
            return push_integer(0);
          }
        };
        Eval_laguerre = function() {
          push(cadr(p1));
          Eval();
          push(caddr(p1));
          Eval();
          push(cadddr(p1));
          Eval();
          p2 = pop();
          if (p2 === symbol(NIL)) {
            push_integer(0);
          } else {
            push(p2);
          }
          return laguerre();
        };
        laguerre = function() {
          var n9;
          n9 = 0;
          save();
          p3 = pop();
          p2 = pop();
          p1 = pop();
          push(p2);
          n9 = pop_integer();
          if (n9 < 0 || isNaN(n9)) {
            push_symbol(LAGUERRE);
            push(p1);
            push(p2);
            push(p3);
            list(4);
            restore();
            return;
          }
          if (issymbol(p1)) {
            laguerre2(n9);
          } else {
            p4 = p1;
            p1 = symbol(SECRETX);
            laguerre2(n9);
            p1 = p4;
            push(symbol(SECRETX));
            push(p1);
            subst();
            Eval();
          }
          return restore();
        };
        laguerre2 = function(n9) {
          var i5, o12, ref2, results;
          i5 = 0;
          push_integer(1);
          push_integer(0);
          p6 = pop();
          results = [];
          for (i5 = o12 = 0, ref2 = n9; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            p5 = p6;
            p6 = pop();
            push_integer(2 * i5 + 1);
            push(p1);
            subtract();
            push(p3);
            add();
            push(p6);
            multiply();
            push_integer(i5);
            push(p3);
            add();
            push(p5);
            multiply();
            subtract();
            push_integer(i5 + 1);
            results.push(divide());
          }
          return results;
        };
        Eval_lcm = function() {
          var results;
          p1 = cdr(p1);
          push(car(p1));
          Eval();
          p1 = cdr(p1);
          results = [];
          while (iscons(p1)) {
            push(car(p1));
            Eval();
            lcm();
            results.push(p1 = cdr(p1));
          }
          return results;
        };
        lcm = function() {
          var prev_expanding;
          prev_expanding = expanding;
          save();
          yylcm();
          restore();
          return expanding = prev_expanding;
        };
        yylcm = function() {
          expanding = 1;
          p2 = pop();
          p1 = pop();
          push(p1);
          push(p2);
          gcd();
          push(p1);
          divide();
          push(p2);
          divide();
          return inverse();
        };
        Eval_leading = function() {
          push(cadr(p1));
          Eval();
          push(caddr(p1));
          Eval();
          p1 = pop();
          if (p1 === symbol(NIL)) {
            guess();
          } else {
            push(p1);
          }
          return leading();
        };
        leading = function() {
          save();
          p2 = pop();
          p1 = pop();
          push(p1);
          push(p2);
          degree();
          p3 = pop();
          push(p1);
          push(p2);
          push(p3);
          power();
          divide();
          push(p2);
          filter();
          return restore();
        };
        Eval_legendre = function() {
          push(cadr(p1));
          Eval();
          push(caddr(p1));
          Eval();
          push(cadddr(p1));
          Eval();
          p2 = pop();
          if (p2 === symbol(NIL)) {
            push_integer(0);
          } else {
            push(p2);
          }
          return legendre();
        };
        legendre = function() {
          save();
          __legendre();
          return restore();
        };
        __legendre = function() {
          var m3, n9;
          m3 = 0;
          n9 = 0;
          p3 = pop();
          p2 = pop();
          p1 = pop();
          push(p2);
          n9 = pop_integer();
          push(p3);
          m3 = pop_integer();
          if (n9 < 0 || isNaN(n9) || m3 < 0 || isNaN(m3)) {
            push_symbol(LEGENDRE);
            push(p1);
            push(p2);
            push(p3);
            list(4);
            return;
          }
          if (issymbol(p1)) {
            __legendre2(n9, m3);
          } else {
            p4 = p1;
            p1 = symbol(SECRETX);
            __legendre2(n9, m3);
            p1 = p4;
            push(symbol(SECRETX));
            push(p1);
            subst();
            Eval();
          }
          return __legendre3(m3);
        };
        __legendre2 = function(n9, m3) {
          var i5, i12, o12, ref2, ref12, results;
          i5 = 0;
          push_integer(1);
          push_integer(0);
          p6 = pop();
          for (i5 = o12 = 0, ref2 = n9; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            p5 = p6;
            p6 = pop();
            push_integer(2 * i5 + 1);
            push(p1);
            multiply();
            push(p6);
            multiply();
            push_integer(i5);
            push(p5);
            multiply();
            subtract();
            push_integer(i5 + 1);
            divide();
          }
          results = [];
          for (i5 = i12 = 0, ref12 = m3; 0 <= ref12 ? i12 < ref12 : i12 > ref12; i5 = 0 <= ref12 ? ++i12 : --i12) {
            push(p1);
            results.push(derivative());
          }
          return results;
        };
        __legendre3 = function(m3) {
          if (m3 === 0) {
            return;
          }
          if (car(p1) === symbol(COS)) {
            push(cadr(p1));
            sine();
            square();
          } else if (car(p1) === symbol(SIN)) {
            push(cadr(p1));
            cosine();
            square();
          } else {
            push_integer(1);
            push(p1);
            square();
            subtract();
          }
          push_integer(m3);
          push_rational(1, 2);
          multiply();
          power();
          multiply();
          if (m3 % 2) {
            return negate();
          }
        };
        list = function(n9) {
          var listIterator, o12, ref2, results;
          listIterator = 0;
          push(symbol(NIL));
          results = [];
          for (listIterator = o12 = 0, ref2 = n9; 0 <= ref2 ? o12 < ref2 : o12 > ref2; listIterator = 0 <= ref2 ? ++o12 : --o12) {
            results.push(cons());
          }
          return results;
        };
        Eval_log = function() {
          push(cadr(p1));
          Eval();
          return logarithm();
        };
        logarithm = function() {
          save();
          yylog();
          return restore();
        };
        yylog = function() {
          var d3;
          d3 = 0;
          p1 = pop();
          if (p1 === symbol(E)) {
            push_integer(1);
            return;
          }
          if (equaln(p1, 1)) {
            push_integer(0);
            return;
          }
          if (isnegativenumber(p1)) {
            push(p1);
            negate();
            logarithm();
            push(imaginaryunit);
            if (evaluatingAsFloats) {
              push_double(Math.PI);
            } else {
              push_symbol(PI);
            }
            multiply();
            add();
            return;
          }
          if (isdouble(p1)) {
            d3 = Math.log(p1.d);
            push_double(d3);
            return;
          }
          if (isfraction(p1)) {
            push(p1);
            numerator();
            logarithm();
            push(p1);
            denominator();
            logarithm();
            subtract();
            return;
          }
          if (car(p1) === symbol(POWER)) {
            push(caddr(p1));
            push(cadr(p1));
            logarithm();
            multiply();
            return;
          }
          if (car(p1) === symbol(MULTIPLY)) {
            push_integer(0);
            p1 = cdr(p1);
            while (iscons(p1)) {
              push(car(p1));
              logarithm();
              add();
              p1 = cdr(p1);
            }
            return;
          }
          push_symbol(LOG);
          push(p1);
          return list(2);
        };
        Eval_lookup = function() {
          p1 = cadr(p1);
          if (!iscons(p1) && cadr(p1).k === SYM) {
            p1 = get_binding(p1);
          }
          return push(p1);
        };
        madd = function(a4, b2) {
          return a4.add(b2);
        };
        msub = function(a4, b2) {
          return a4.subtract(b2);
        };
        addf = function(a4, b2) {
          return a4.add(b2);
        };
        subf = function(a4, b2) {
          return a4.subtract(b2);
        };
        ucmp = function(a4, b2) {
          return a4.compareAbs(b2);
        };
        mgcd = function(u4, v2) {
          return bigInt.gcd(u4, v2);
        };
        new_string = function(s7) {
          var theNewString;
          theNewString = new U();
          theNewString.k = STR;
          theNewString.str = s7;
          return theNewString;
        };
        out_of_memory = function() {
          return stop("out of memory");
        };
        push_zero_matrix = function(i5, j2) {
          push(alloc_tensor(i5 * j2));
          stack[tos - 1].tensor.ndim = 2;
          stack[tos - 1].tensor.dim[0] = i5;
          return stack[tos - 1].tensor.dim[1] = j2;
        };
        push_identity_matrix = function(n9) {
          var i5, o12, ref2;
          push_zero_matrix(n9, n9);
          for (i5 = o12 = 0, ref2 = n9; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            stack[tos - 1].tensor.elem[i5 * n9 + i5] = one;
          }
          return check_tensor_dimensions(stack[tos - 1]);
        };
        push_cars = function(p11) {
          var results;
          results = [];
          while (iscons(p11)) {
            push(car(p11));
            results.push(p11 = cdr(p11));
          }
          return results;
        };
        equal = function(p12, p22) {
          if (cmp_expr(p12, p22) === 0) {
            return 1;
          } else {
            return 0;
          }
        };
        lessp = function(p12, p22) {
          if (cmp_expr(p12, p22) < 0) {
            return 1;
          } else {
            return 0;
          }
        };
        sign = function(n9) {
          if (n9 < 0) {
            return -1;
          } else if (n9 > 0) {
            return 1;
          } else {
            return 0;
          }
        };
        cmp_expr = function(p12, p22) {
          var n9;
          n9 = 0;
          if (p12 === p22) {
            return 0;
          }
          if (p12 === symbol(NIL)) {
            return -1;
          }
          if (p22 === symbol(NIL)) {
            return 1;
          }
          if (isNumericAtom(p12) && isNumericAtom(p22)) {
            return sign(compare_numbers(p12, p22));
          }
          if (isNumericAtom(p12)) {
            return -1;
          }
          if (isNumericAtom(p22)) {
            return 1;
          }
          if (isstr(p12) && isstr(p22)) {
            return sign(strcmp(p12.str, p22.str));
          }
          if (isstr(p12)) {
            return -1;
          }
          if (isstr(p22)) {
            return 1;
          }
          if (issymbol(p12) && issymbol(p22)) {
            return sign(strcmp(get_printname(p12), get_printname(p22)));
          }
          if (issymbol(p12)) {
            return -1;
          }
          if (issymbol(p22)) {
            return 1;
          }
          if (istensor(p12) && istensor(p22)) {
            return compare_tensors(p12, p22);
          }
          if (istensor(p12)) {
            return -1;
          }
          if (istensor(p22)) {
            return 1;
          }
          while (iscons(p12) && iscons(p22)) {
            n9 = cmp_expr(car(p12), car(p22));
            if (n9 !== 0) {
              return n9;
            }
            p12 = cdr(p12);
            p22 = cdr(p22);
          }
          if (iscons(p22)) {
            return -1;
          }
          if (iscons(p12)) {
            return 1;
          }
          return 0;
        };
        length = function(p11) {
          var n9;
          n9 = 0;
          while (iscons(p11)) {
            p11 = cdr(p11);
            n9++;
          }
          return n9;
        };
        unique = function(p11) {
          save();
          p1 = symbol(NIL);
          p2 = symbol(NIL);
          unique_f(p11);
          if (p2 !== symbol(NIL)) {
            p1 = symbol(NIL);
          }
          p11 = p1;
          restore();
          return p11;
        };
        unique_f = function(p11) {
          if (isstr(p11)) {
            if (p1 === symbol(NIL)) {
              p1 = p11;
            } else if (p11 !== p1) {
              p2 = p11;
            }
            return;
          }
          while (iscons(p11)) {
            unique_f(car(p11));
            if (p2 !== symbol(NIL)) {
              return;
            }
            p11 = cdr(p11);
          }
        };
        ssqrt = function() {
          push_rational(1, 2);
          return power();
        };
        yyexpand = function() {
          var prev_expanding;
          prev_expanding = expanding;
          expanding = 1;
          Eval();
          return expanding = prev_expanding;
        };
        exponential = function() {
          push_symbol(E);
          swap();
          return power();
        };
        square = function() {
          push_integer(2);
          return power();
        };
        sort_stack = function(n9) {
          var h5, subsetOfStack;
          h5 = tos - n9;
          subsetOfStack = stack.slice(h5, h5 + n9);
          subsetOfStack.sort(cmp_expr);
          return stack = stack.slice(0, h5).concat(subsetOfStack).concat(stack.slice(h5 + n9));
        };
        $.equal = equal;
        $.length = length;
        mmul = function(a4, b2) {
          return a4.multiply(b2);
        };
        mdiv = function(a4, b2) {
          return a4.divide(b2);
        };
        mmod = function(a4, b2) {
          return a4.mod(b2);
        };
        mdivrem = function(a4, b2) {
          var toReturn;
          toReturn = a4.divmod(b2);
          return [toReturn.quotient, toReturn.remainder];
        };
        Eval_mod = function() {
          push(cadr(p1));
          Eval();
          push(caddr(p1));
          Eval();
          return mod();
        };
        mod = function() {
          var n9;
          n9 = 0;
          save();
          p2 = pop();
          p1 = pop();
          if (isZeroAtomOrTensor(p2)) {
            stop("mod function: divide by zero");
          }
          if (!isNumericAtom(p1) || !isNumericAtom(p2)) {
            push_symbol(MOD);
            push(p1);
            push(p2);
            list(3);
            restore();
            return;
          }
          if (isdouble(p1)) {
            push(p1);
            n9 = pop_integer();
            if (isNaN(n9)) {
              stop("mod function: cannot convert float value to integer");
            }
            push_integer(n9);
            p1 = pop();
          }
          if (isdouble(p2)) {
            push(p2);
            n9 = pop_integer();
            if (isNaN(n9)) {
              stop("mod function: cannot convert float value to integer");
            }
            push_integer(n9);
            p2 = pop();
          }
          if (!isinteger(p1) || !isinteger(p2)) {
            stop("mod function: integer arguments expected");
          }
          p3 = new U();
          p3.k = NUM;
          p3.q.a = mmod(p1.q.a, p2.q.a);
          p3.q.b = mint(1);
          push(p3);
          return restore();
        };
        mpow = function(a4, n9) {
          return a4.pow(n9);
        };
        mprime = function(n9) {
          return n9.isProbablePrime();
        };
        mroot = function(n9, index) {
          var i5, j2, k2, o12, ref2, x2, y2;
          n9 = n9.abs();
          i5 = 0;
          j2 = 0;
          k2 = 0;
          if (index === 0) {
            stop("root index is zero");
          }
          k2 = 0;
          while (n9.shiftRight(k2) > 0) {
            k2++;
          }
          if (k2 === 0) {
            return mint(0);
          }
          k2 = Math.floor((k2 - 1) / index);
          j2 = Math.floor(k2 / 32 + 1);
          x2 = bigInt(j2);
          for (i5 = o12 = 0, ref2 = j2; 0 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 0 <= ref2 ? ++o12 : --o12) {
            x2 = x2.and(bigInt(1).shiftLeft(i5).not());
          }
          while (k2 >= 0) {
            x2 = x2.or(bigInt(1).shiftLeft(k2));
            y2 = mpow(x2, index);
            switch (mcmp(y2, n9)) {
              case 0:
                return x2;
              case 1:
                x2 = x2.and(bigInt(1).shiftLeft(k2).not());
            }
            k2--;
          }
          return 0;
        };
        DEBUG_MULTIPLY = false;
        Eval_multiply = function() {
          var results;
          push(cadr(p1));
          Eval();
          p1 = cddr(p1);
          results = [];
          while (iscons(p1)) {
            push(car(p1));
            Eval();
            multiply();
            results.push(p1 = cdr(p1));
          }
          return results;
        };
        multiply = function() {
          if (esc_flag) {
            stop("escape key stop");
          }
          if (isNumericAtom(stack[tos - 2]) && isNumericAtom(stack[tos - 1])) {
            return multiply_numbers();
          } else {
            save();
            yymultiply();
            return restore();
          }
        };
        yymultiply = function() {
          var h5, i5, n9, o12, ref2, ref12;
          h5 = 0;
          i5 = 0;
          n9 = 0;
          p2 = pop();
          p1 = pop();
          h5 = tos;
          if (isZeroAtom(p1) || isZeroAtom(p2)) {
            if (evaluatingAsFloats) {
              push_double(0);
            } else {
              push(zero);
            }
            return;
          }
          if (expanding && isadd(p1)) {
            p1 = cdr(p1);
            if (evaluatingAsFloats) {
              push_double(0);
            } else {
              push(zero);
            }
            while (iscons(p1)) {
              push(car(p1));
              push(p2);
              multiply();
              add();
              p1 = cdr(p1);
            }
            return;
          }
          if (expanding && isadd(p2)) {
            p2 = cdr(p2);
            if (evaluatingAsFloats) {
              push_double(0);
            } else {
              push(zero);
            }
            while (iscons(p2)) {
              push(p1);
              push(car(p2));
              multiply();
              add();
              p2 = cdr(p2);
            }
            return;
          }
          if (!istensor(p1) && istensor(p2)) {
            push(p1);
            push(p2);
            scalar_times_tensor();
            return;
          }
          if (istensor(p1) && !istensor(p2)) {
            push(p1);
            push(p2);
            tensor_times_scalar();
            return;
          }
          if (car(p1) === symbol(MULTIPLY)) {
            p1 = cdr(p1);
          } else {
            push(p1);
            list(1);
            p1 = pop();
          }
          if (car(p2) === symbol(MULTIPLY)) {
            p2 = cdr(p2);
          } else {
            push(p2);
            list(1);
            p2 = pop();
          }
          if (isNumericAtom(car(p1)) && isNumericAtom(car(p2))) {
            push(car(p1));
            push(car(p2));
            multiply_numbers();
            p1 = cdr(p1);
            p2 = cdr(p2);
          } else if (isNumericAtom(car(p1))) {
            push(car(p1));
            p1 = cdr(p1);
          } else if (isNumericAtom(car(p2))) {
            push(car(p2));
            p2 = cdr(p2);
          } else {
            if (evaluatingAsFloats) {
              push_double(1);
            } else {
              push(one);
            }
          }
          parse_p1();
          parse_p2();
          while (iscons(p1) && iscons(p2)) {
            if (caar(p1) === symbol(OPERATOR) && caar(p2) === symbol(OPERATOR)) {
              push_symbol(OPERATOR);
              push(cdar(p1));
              push(cdar(p2));
              append();
              cons();
              p1 = cdr(p1);
              p2 = cdr(p2);
              parse_p1();
              parse_p2();
              continue;
            }
            switch (cmp_expr(p3, p4)) {
              case -1:
                push(car(p1));
                p1 = cdr(p1);
                parse_p1();
                break;
              case 1:
                push(car(p2));
                p2 = cdr(p2);
                parse_p2();
                break;
              case 0:
                combine_factors(h5);
                p1 = cdr(p1);
                p2 = cdr(p2);
                parse_p1();
                parse_p2();
                break;
              default:
                stop("internal error 2");
            }
          }
          while (iscons(p1)) {
            push(car(p1));
            p1 = cdr(p1);
          }
          while (iscons(p2)) {
            push(car(p2));
            p2 = cdr(p2);
          }
          __normalize_radical_factors(h5);
          if (expanding) {
            for (i5 = o12 = ref2 = h5, ref12 = tos; ref2 <= ref12 ? o12 < ref12 : o12 > ref12; i5 = ref2 <= ref12 ? ++o12 : --o12) {
              if (isadd(stack[i5])) {
                multiply_all(tos - h5);
                return;
              }
            }
          }
          n9 = tos - h5;
          if (n9 === 1) {
            return;
          }
          if (isrational(stack[h5]) && equaln(stack[h5], 1)) {
            if (n9 === 2) {
              p7 = pop();
              pop();
              push(p7);
            } else {
              stack[h5] = symbol(MULTIPLY);
              list(n9);
            }
            return;
          }
          list(n9);
          p7 = pop();
          push_symbol(MULTIPLY);
          push(p7);
          return cons();
        };
        parse_p1 = function() {
          p3 = car(p1);
          p5 = evaluatingAsFloats ? one_as_double : one;
          if (car(p3) === symbol(POWER)) {
            p5 = caddr(p3);
            return p3 = cadr(p3);
          }
        };
        parse_p2 = function() {
          p4 = car(p2);
          p6 = evaluatingAsFloats ? one_as_double : one;
          if (car(p4) === symbol(POWER)) {
            p6 = caddr(p4);
            return p4 = cadr(p4);
          }
        };
        combine_factors = function(h5) {
          push(p4);
          push(p5);
          push(p6);
          add();
          power();
          p7 = pop();
          if (isNumericAtom(p7)) {
            push(stack[h5]);
            push(p7);
            multiply_numbers();
            return stack[h5] = pop();
          } else if (car(p7) === symbol(MULTIPLY)) {
            if (isNumericAtom(cadr(p7)) && cdddr(p7) === symbol(NIL)) {
              push(stack[h5]);
              push(cadr(p7));
              multiply_numbers();
              stack[h5] = pop();
              return push(caddr(p7));
            } else {
              return push(p7);
            }
          } else {
            return push(p7);
          }
        };
        gp = [[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 1, -6, -7, -8, -3, -4, -5, 13, 14, 15, -16, 9, 10, 11, -12], [0, 0, 6, -1, -11, 10, -2, -15, 14, 12, -5, 4, -9, 16, -8, 7, -13], [0, 0, 7, 11, -1, -9, 15, -2, -13, 5, 12, -3, -10, 8, 16, -6, -14], [0, 0, 8, -10, 9, -1, -14, 13, -2, -4, 3, 12, -11, -7, 6, 16, -15], [0, 0, 3, 2, 15, -14, 1, 11, -10, 16, -8, 7, 13, 12, -5, 4, 9], [0, 0, 4, -15, 2, 13, -11, 1, 9, 8, 16, -6, 14, 5, 12, -3, 10], [0, 0, 5, 14, -13, 2, 10, -9, 1, -7, 6, 16, 15, -4, 3, 12, 11], [0, 0, 13, 12, -5, 4, 16, -8, 7, -1, -11, 10, -3, -2, -15, 14, -6], [0, 0, 14, 5, 12, -3, 8, 16, -6, 11, -1, -9, -4, 15, -2, -13, -7], [0, 0, 15, -4, 3, 12, -7, 6, 16, -10, 9, -1, -5, -14, 13, -2, -8], [0, 0, 16, -9, -10, -11, -13, -14, -15, -3, -4, -5, 1, -6, -7, -8, 2], [0, 0, 9, -16, 8, -7, -12, 5, -4, -2, -15, 14, 6, -1, -11, 10, 3], [0, 0, 10, -8, -16, 6, -5, -12, 3, 15, -2, -13, 7, 11, -1, -9, 4], [0, 0, 11, 7, -6, -16, 4, -3, -12, -14, 13, -2, 8, -10, 9, -1, 5], [0, 0, 12, 13, 14, 15, 9, 10, 11, -6, -7, -8, -2, -3, -4, -5, -1]];
        combine_gammas = function(h5) {
          var n9;
          n9 = gp[Math.floor(p1.gamma)][Math.floor(p2.gamma)];
          if (n9 < 0) {
            n9 = -n9;
            push(stack[h5]);
            negate();
            stack[h5] = pop();
          }
          if (n9 > 1) {
            return push(_gamma[n9]);
          }
        };
        multiply_noexpand = function() {
          var prev_expanding;
          prev_expanding = expanding;
          expanding = 0;
          multiply();
          return expanding = prev_expanding;
        };
        multiply_all = function(n9) {
          var h5, i5, o12, ref2;
          i5 = 0;
          if (n9 === 1) {
            return;
          }
          if (n9 === 0) {
            push(evaluatingAsFloats ? one_as_double : one);
            return;
          }
          h5 = tos - n9;
          push(stack[h5]);
          for (i5 = o12 = 1, ref2 = n9; 1 <= ref2 ? o12 < ref2 : o12 > ref2; i5 = 1 <= ref2 ? ++o12 : --o12) {
            push(stack[h5 + i5]);
            multiply();
          }
          stack[h5] = pop();
          return moveTos(h5 + 1);
        };
        multiply_all_noexpand = function(n9) {
          var prev_expanding;
          prev_expanding = expanding;
          expanding = 0;
          multiply_all(n9);
          return expanding = prev_expanding;
        };
        divide = function() {
          if (isNumericAtom(stack[tos - 2]) && isNumericAtom(stack[tos - 1])) {
            return divide_numbers();
          } else {
            inverse();
            return multiply();
          }
        };
        inverse = function() {
          if (isNumericAtom(stack[tos - 1])) {
            return invert_number();
          } else {
            push_integer(-1);
            return power();
          }
        };
        reciprocate = function() {
          return inverse();
        };
        negate = function() {
          if (isNumericAtom(stack[tos - 1])) {
            return negate_number();
          } else {
            if (evaluatingAsFloats) {
              push_double(-1);
            } else {
              push_integer(-1);
            }
            return multiply();
          }
        };
        negate_expand = function() {
          var prev_expanding;
          prev_expanding = expanding;
          expanding = 1;
          negate();
          return expanding = prev_expanding;
        };
        negate_noexpand = function() {
          var prev_expanding;
          prev_expanding = expanding;
          expanding = 0;
          negate();
          return expanding = prev_expanding;
        };
        __normalize_radical_factors = function(h5) {
          var i5, i12, j12, o12, ref2, ref12, ref22, ref3, ref4, ref5;
          i5 = 0;
          if (isplusone(stack[h5]) || isminusone(stack[h5]) || isdouble(stack[h5])) {
            return;
          }
          for (i5 = o12 = ref2 = h5 + 1, ref12 = tos; ref2 <= ref12 ? o12 < ref12 : o12 > ref12; i5 = ref2 <= ref12 ? ++o12 : --o12) {
            if (__is_radical_number(stack[i5])) {
              break;
            }
          }
          if (i5 === tos) {
            return;
          }
          save();
          push(stack[h5]);
          mp_numerator();
          if (DEBUG_MULTIPLY) {
            console.log("__normalize_radical_factors numerator: " + stack[tos - 1]);
          }
          p1 = pop();
          for (i5 = i12 = ref22 = h5 + 1, ref3 = tos; ref22 <= ref3 ? i12 < ref3 : i12 > ref3; i5 = ref22 <= ref3 ? ++i12 : --i12) {
            if (isplusone(p1) || isminusone(p1)) {
              break;
            }
            if (!__is_radical_number(stack[i5])) {
              continue;
            }
            p3 = cadr(stack[i5]);
            p4 = caddr(stack[i5]);
            if (!isnegativenumber(p4)) {
              continue;
            }
            push(p1);
            push(p3);
            divide();
            p5 = pop();
            if (!isinteger(p5)) {
              continue;
            }
            p1 = p5;
            push_symbol(POWER);
            push(p3);
            push(evaluatingAsFloats ? one_as_double : one);
            push(p4);
            add();
            list(3);
            stack[i5] = pop();
          }
          push(stack[h5]);
          mp_denominator();
          if (DEBUG_MULTIPLY) {
            console.log("__normalize_radical_factors denominator: " + stack[tos - 1]);
          }
          p2 = pop();
          for (i5 = j12 = ref4 = h5 + 1, ref5 = tos; ref4 <= ref5 ? j12 < ref5 : j12 > ref5; i5 = ref4 <= ref5 ? ++j12 : --j12) {
            if (isplusone(p2)) {
              break;
            }
            if (!__is_radical_number(stack[i5])) {
              continue;
            }
            p3 = cadr(stack[i5]);
            p4 = caddr(stack[i5]);
            if (isnegativenumber(p4)) {
              continue;
            }
            push(p2);
            push(p3);
            divide();
            p5 = pop();
            if (!isinteger(p5)) {
              continue;
            }
            if (DEBUG_MULTIPLY) {
              console.log("__new radical p5: " + p5.toString());
            }
            if (DEBUG_MULTIPLY) {
              console.log("__new radical top stack: " + stack[tos - 1]);
            }
            p2 = p5;
            push_symbol(POWER);
            push(p3);
            push(p4);
            if (DEBUG_MULTIPLY) {
              console.log("__new radical p3: " + p3.toString());
            }
            if (DEBUG_MULTIPLY) {
              console.log("__new radical p4: " + p4.toString());
            }
            push(one);
            subtract();
            if (dontCreateNewRadicalsInDenominatorWhenEvalingMultiplication) {
              if (isinteger(p3) && !isinteger(stack[tos - 1]) && isnegativenumber(stack[tos - 1])) {
                pop();
                pop();
                pop();
                push(p1);
                push(p3);
                divide();
                p1 = pop();
                break;
              }
            }
            if (DEBUG_MULTIPLY) {
              console.log("__new radical exponent: " + stack[tos - 1]);
            }
            list(3);
            stack[i5] = pop();
          }
          push(p1);
          push(p2);
          divide();
          stack[h5] = pop();
          return restore();
        };
        __is_radical_number = function(p11) {
          return car(p11) === symbol(POWER) && isNumericAtom(cadr(p11)) && isfraction(caddr(p11)) && !isminusone(cadr(p11));
        };
        NROOTS_YMAX = 101;
        NROOTS_DELTA = 1e-6;
        NROOTS_EPSILON = 1e-9;
        NROOTS_ABS = function(z2) {
          return Math.sqrt(z2.r * z2.r + z2.i * z2.i);
        };
        theRandom = 0;
        NROOTS_RANDOM = function() {
          return 4 * Math.random() - 2;
        };
        numericRootOfPolynomial = function() {
          class numericRootOfPolynomial2 {
          }
          ;
          numericRootOfPolynomial2.prototype.r = 0;
          numericRootOfPolynomial2.prototype.i = 0;
          return numericRootOfPolynomial2;
        }.call(this);
        nroots_a = new numericRootOfPolynomial();
        nroots_b = new numericRootOfPolynomial();
        nroots_x = new numericRootOfPolynomial();
        nroots_y = new numericRootOfPolynomial();
        nroots_fa = new numericRootOfPolynomial();
        nroots_fb = new numericRootOfPolynomial();
        nroots_dx = new numericRootOfPolynomial();
        nroots_df = new numericRootOfPolynomial();
        nroots_c = [];
        for (initNRoots = o = 0, ref = NROOTS_YMAX; 0 <= ref ? o < ref : o > ref; initNRoots = 0 <= ref ? ++o : --o) {
          nroots_c[initNRoots] = new numericRootOfPolynomial();
        }
        Eval_nroots = function() {
          var h5, i5, i12, j12, k2, l1, n9, ref12, ref2, ref3;
          h5 = 0;
          i5 = 0;
          k2 = 0;
          n9 = 0;
          push(cadr(p1));
          Eval();
          push(caddr(p1));
          Eval();
          p2 = pop();
          if (p2 === symbol(NIL)) {
            guess();
          } else {
            push(p2);
          }
          p2 = pop();
          p1 = pop();
          if (!ispolyexpandedform(p1, p2)) {
            stop("nroots: polynomial?");
          }
          h5 = tos;
          n9 = coeff(p2, p1);
          if (n9 > NROOTS_YMAX) {
            stop("nroots: degree?");
          }
          for (i5 = i12 = 0, ref12 = n9; 0 <= ref12 ? i12 < ref12 : i12 > ref12; i5 = 0 <= ref12 ? ++i12 : --i12) {
            push(stack[h5 + i5]);
            real();
            yyfloat();
            Eval();
            p1 = pop();
            push(stack[h5 + i5]);
            imag();
            yyfloat();
            Eval();
            p2 = pop();
            if (!isdouble(p1) || !isdouble(p2)) {
              stop("nroots: coefficients?");
            }
            nroots_c[i5].r = p1.d;
            nroots_c[i5].i = p2.d;
          }
          moveTos(h5);
          monic(n9);
          for (k2 = j12 = ref2 = n9; j12 > 1; k2 = j12 += -1) {
            findroot(k2);
            if (Math.abs(nroots_a.r) < NROOTS_DELTA) {
              nroots_a.r = 0;
            }
            if (Math.abs(nroots_a.i) < NROOTS_DELTA) {
              nroots_a.i = 0;
            }
            push_double(nroots_a.r);
            push_double(nroots_a.i);
            push(imaginaryunit);
            multiply();
            add();
            NROOTS_divpoly(k2);
          }
          n9 = tos - h5;
          if (n9 > 1) {
            sort_stack(n9);
            p1 = alloc_tensor(n9);
            p1.tensor.ndim = 1;
            p1.tensor.dim[0] = n9;
            for (i5 = l1 = 0, ref3 = n9; 0 <= ref3 ? l1 < ref3 : l1 > ref3; i5 = 0 <= ref3 ? ++l1 : --l1) {
              p1.tensor.elem[i5] = stack[h5 + i5];
            }
            moveTos(h5);
            return push(p1);
          }
        };
        monic = function(n9) {
          var i12, k2, ref12, t5;
          k2 = 0;
          t5 = 0;
          nroots_y.r = nroots_c[n9 - 1].r;
          nroots_y.i = nroots_c[n9 - 1].i;
          t5 = nroots_y.r * nroots_y.r + nroots_y.i * nroots_y.i;
          for (k2 = i12 = 0, ref12 = n9 - 1; 0 <= ref12 ? i12 < ref12 : i12 > ref12; k2 = 0 <= ref12 ? ++i12 : --i12) {
            nroots_c[k2].r = (nroots_c[k2].r * nroots_y.r + nroots_c[k2].i * nroots_y.i) / t5;
            nroots_c[k2].i = (nroots_c[k2].i * nroots_y.r - nroots_c[k2].r * nroots_y.i) / t5;
          }
          nroots_c[n9 - 1].r = 1;
          return nroots_c[n9 - 1].i = 0;
        };
        findroot = function(n9) {
          var i12, j2, j12, k2, nrabs, t5;
          j2 = 0;
          k2 = 0;
          t5 = 0;
          if (NROOTS_ABS(nroots_c[0]) < NROOTS_DELTA) {
            nroots_a.r = 0;
            nroots_a.i = 0;
            return;
          }
          for (j2 = i12 = 0; i12 < 100; j2 = ++i12) {
            nroots_a.r = NROOTS_RANDOM();
            nroots_a.i = NROOTS_RANDOM();
            compute_fa(n9);
            nroots_b.r = nroots_a.r;
            nroots_b.i = nroots_a.i;
            nroots_fb.r = nroots_fa.r;
            nroots_fb.i = nroots_fa.i;
            nroots_a.r = NROOTS_RANDOM();
            nroots_a.i = NROOTS_RANDOM();
            for (k2 = j12 = 0; j12 < 1e3; k2 = ++j12) {
              compute_fa(n9);
              nrabs = NROOTS_ABS(nroots_fa);
              if (DEBUG) {
                console.log("nrabs: " + nrabs);
              }
              if (nrabs < NROOTS_EPSILON) {
                return;
              }
              if (NROOTS_ABS(nroots_fa) < NROOTS_ABS(nroots_fb)) {
                nroots_x.r = nroots_a.r;
                nroots_x.i = nroots_a.i;
                nroots_a.r = nroots_b.r;
                nroots_a.i = nroots_b.i;
                nroots_b.r = nroots_x.r;
                nroots_b.i = nroots_x.i;
                nroots_x.r = nroots_fa.r;
                nroots_x.i = nroots_fa.i;
                nroots_fa.r = nroots_fb.r;
                nroots_fa.i = nroots_fb.i;
                nroots_fb.r = nroots_x.r;
                nroots_fb.i = nroots_x.i;
              }
              nroots_dx.r = nroots_b.r - nroots_a.r;
              nroots_dx.i = nroots_b.i - nroots_a.i;
              nroots_df.r = nroots_fb.r - nroots_fa.r;
              nroots_df.i = nroots_fb.i - nroots_fa.i;
              t5 = nroots_df.r * nroots_df.r + nroots_df.i * nroots_df.i;
              if (t5 === 0) {
                break;
              }
              nroots_y.r = (nroots_dx.r * nroots_df.r + nroots_dx.i * nroots_df.i) / t5;
              nroots_y.i = (nroots_dx.i * nroots_df.r - nroots_dx.r * nroots_df.i) / t5;
              nroots_a.r = nroots_b.r - (nroots_y.r * nroots_fb.r - nroots_y.i * nroots_fb.i);
              nroots_a.i = nroots_b.i - (nroots_y.r * nroots_fb.i + nroots_y.i * nroots_fb.r);
            }
          }
          return stop("nroots: convergence error");
        };
        compute_fa = function(n9) {
          var i12, k2, ref12, results, t5;
          k2 = 0;
          t5 = 0;
          nroots_x.r = nroots_a.r;
          nroots_x.i = nroots_a.i;
          nroots_fa.r = nroots_c[0].r + nroots_c[1].r * nroots_x.r - nroots_c[1].i * nroots_x.i;
          nroots_fa.i = nroots_c[0].i + nroots_c[1].r * nroots_x.i + nroots_c[1].i * nroots_x.r;
          results = [];
          for (k2 = i12 = 2, ref12 = n9; 2 <= ref12 ? i12 < ref12 : i12 > ref12; k2 = 2 <= ref12 ? ++i12 : --i12) {
            t5 = nroots_a.r * nroots_x.r - nroots_a.i * nroots_x.i;
            nroots_x.i = nroots_a.r * nroots_x.i + nroots_a.i * nroots_x.r;
            nroots_x.r = t5;
            nroots_fa.r += nroots_c[k2].r * nroots_x.r - nroots_c[k2].i * nroots_x.i;
            results.push(nroots_fa.i += nroots_c[k2].r * nroots_x.i + nroots_c[k2].i * nroots_x.r);
          }
          return results;
        };
        NROOTS_divpoly = function(n9) {
          var i12, j12, k2, ref12, ref2, results;
          k2 = 0;
          for (k2 = i12 = ref12 = n9 - 1; ref12 <= 0 ? i12 < 0 : i12 > 0; k2 = ref12 <= 0 ? ++i12 : --i12) {
            nroots_c[k2 - 1].r += nroots_c[k2].r * nroots_a.r - nroots_c[k2].i * nroots_a.i;
            nroots_c[k2 - 1].i += nroots_c[k2].i * nroots_a.r + nroots_c[k2].r * nroots_a.i;
          }
          if (NROOTS_ABS(nroots_c[0]) > NROOTS_DELTA) {
            stop("nroots: residual error");
          }
          results = [];
          for (k2 = j12 = 0, ref2 = n9 - 1; 0 <= ref2 ? j12 < ref2 : j12 > ref2; k2 = 0 <= ref2 ? ++j12 : --j12) {
            nroots_c[k2].r = nroots_c[k2 + 1].r;
            results.push(nroots_c[k2].i = nroots_c[k2 + 1].i);
          }
          return results;
        };
        Eval_numerator = function() {
          push(cadr(p1));
          Eval();
          return numerator();
        };
        numerator = function() {
          var h5, theArgument2;
          h5 = 0;
          theArgument2 = pop();
          if (car(theArgument2) === symbol(ADD)) {
            push(theArgument2);
            rationalize();
            theArgument2 = pop();
          }
          if (car(theArgument2) === symbol(MULTIPLY) && !isplusone(car(cdr(theArgument2)))) {
            h5 = tos;
            theArgument2 = cdr(theArgument2);
            while (iscons(theArgument2)) {
              push(car(theArgument2));
              numerator();
              theArgument2 = cdr(theArgument2);
            }
            return multiply_all(tos - h5);
          } else if (isrational(theArgument2)) {
            push(theArgument2);
            return mp_numerator();
          } else if (car(theArgument2) === symbol(POWER) && isnegativeterm(caddr(theArgument2))) {
            return push(one);
          } else {
            return push(theArgument2);
          }
        };
        Eval_outer = function() {
          var results;
          p1 = cdr(p1);
          push(car(p1));
          Eval();
          p1 = cdr(p1);
          results = [];
          while (iscons(p1)) {
            push(car(p1));
            Eval();
            outer();
            results.push(p1 = cdr(p1));
          }
          return results;
        };
        outer = function() {
          save();
          p2 = pop();
          p1 = pop();
          if (istensor(p1) && istensor(p2)) {
            yyouter();
          } else {
            push(p1);
            push(p2);
            if (istensor(p1)) {
              tensor_times_scalar();
            } else if (istensor(p2)) {
              scalar_times_tensor();
            } else {
              multiply();
            }
          }
          return restore();
        };
        yyouter = function() {
          var i5, i12, j2, j12, k2, l1, m1, ndim, nelem, ref12, ref2, ref3, ref4;
          i5 = 0;
          j2 = 0;
          k2 = 0;
          ndim = 0;
          nelem = 0;
          ndim = p1.tensor.ndim + p2.tensor.ndim;
          if (ndim > MAXDIM) {
            stop("outer: rank of result exceeds maximum");
          }
          nelem = p1.tensor.nelem * p2.tensor.nelem;
          p3 = alloc_tensor(nelem);
          p3.tensor.ndim = ndim;
          for (i5 = i12 = 0, ref12 = p1.tensor.ndim; 0 <= ref12 ? i12 < ref12 : i12 > ref12; i5 = 0 <= ref12 ? ++i12 : --i12) {
            p3.tensor.dim[i5] = p1.tensor.dim[i5];
          }
          j2 = i5;
          for (i5 = j12 = 0, ref2 = p2.tensor.ndim; 0 <= ref2 ? j12 < ref2 : j12 > ref2; i5 = 0 <= ref2 ? ++j12 : --j12) {
            p3.tensor.dim[j2 + i5] = p2.tensor.dim[i5];
          }
          k2 = 0;
          for (i5 = l1 = 0, ref3 = p1.tensor.nelem; 0 <= ref3 ? l1 < ref3 : l1 > ref3; i5 = 0 <= ref3 ? ++l1 : --l1) {
            for (j2 = m1 = 0, ref4 = p2.tensor.nelem; 0 <= ref4 ? m1 < ref4 : m1 > ref4; j2 = 0 <= ref4 ? ++m1 : --m1) {
              push(p1.tensor.elem[i5]);
              push(p2.tensor.elem[j2]);
              multiply();
              p3.tensor.elem[k2++] = pop();
            }
          }
          return push(p3);
        };
        partition = function() {
          save();
          p2 = pop();
          p1 = pop();
          push_integer(1);
          p3 = pop();
          p4 = p3;
          p1 = cdr(p1);
          while (iscons(p1)) {
            if (Find(car(p1), p2)) {
              push(p4);
              push(car(p1));
              multiply();
              p4 = pop();
            } else {
              push(p3);
              push(car(p1));
              multiply();
              p3 = pop();
            }
            p1 = cdr(p1);
          }
          push(p3);
          push(p4);
          return restore();
        };
        Eval_silentpattern = function() {
          Eval_pattern();
          pop();
          return push_symbol(NIL);
        };
        Eval_pattern = function() {
          var firstArgument, patternPosition, secondArgument, stringKey, thirdArgument;
          if (!iscons(cdr(p1))) {
            stop("pattern needs at least a template and a transformed version");
          }
          firstArgument = car(cdr(p1));
          secondArgument = car(cdr(cdr(p1)));
          if (secondArgument === symbol(NIL)) {
            stop("pattern needs at least a template and a transformed version");
          }
          if (!iscons(cdr(cdr(p1)))) {
            thirdArgument = symbol(NIL);
          } else {
            thirdArgument = car(cdr(cdr(cdr(p1))));
          }
          if (equal(firstArgument, secondArgument)) {
            stop("recursive pattern");
          }
          stringKey = "template: " + print_list(firstArgument);
          stringKey += " tests: " + print_list(thirdArgument);
          if (DEBUG) {
            console.log("pattern stringkey: " + stringKey);
          }
          patternPosition = userSimplificationsInStringForm.indexOf(stringKey);
          if (patternPosition === -1) {
            userSimplificationsInStringForm.push(stringKey);
            userSimplificationsInListForm.push(cdr(p1));
          } else {
            if (DEBUG) {
              console.log("pattern already exists, replacing. " + cdr(p1));
            }
            userSimplificationsInStringForm[patternPosition] = stringKey;
            userSimplificationsInListForm[patternPosition] = cdr(p1);
          }
          push_symbol(PATTERN);
          push(cdr(p1));
          return list(2);
        };
        do_clearPatterns = function() {
          userSimplificationsInListForm = [];
          return userSimplificationsInStringForm = [];
        };
        Eval_clearpatterns = function() {
          do_clearPatterns();
          return push_symbol(NIL);
        };
        Eval_patternsinfo = function() {
          var patternsinfoToBePrinted;
          patternsinfoToBePrinted = patternsinfo();
          if (patternsinfoToBePrinted !== "") {
            return push(new_string(patternsinfoToBePrinted));
          } else {
            return push_symbol(NIL);
          }
        };
        patternsinfo = function() {
          var i5, i12, len, patternsinfoToBePrinted;
          patternsinfoToBePrinted = "";
          for (i12 = 0, len = userSimplificationsInListForm.length; i12 < len; i12++) {
            i5 = userSimplificationsInListForm[i12];
            patternsinfoToBePrinted += userSimplificationsInListForm + "\n";
          }
          return patternsinfoToBePrinted;
        };
        Eval_polar = function() {
          push(cadr(p1));
          Eval();
          return polar();
        };
        polar = function() {
          evaluatingPolar++;
          save();
          p1 = pop();
          push(p1);
          abs();
          push(imaginaryunit);
          push(p1);
          arg();
          multiply();
          exponential();
          multiply();
          evaluatingPolar--;
          return restore();
        };
        n_factor_number = 0;
        factor_number = function() {
          var h5;
          h5 = 0;
          save();
          p1 = pop();
          if (equaln(p1, 0) || equaln(p1, 1) || equaln(p1, -1)) {
            push(p1);
            restore();
            return;
          }
          n_factor_number = p1.q.a;
          h5 = tos;
          factor_a();
          if (tos - h5 > 1) {
            list(tos - h5);
            push_symbol(MULTIPLY);
            swap();
            cons();
          }
          return restore();
        };
        factor_a = function() {
          var i12, k2;
          k2 = 0;
          if (n_factor_number.isNegative()) {
            n_factor_number = setSignTo(n_factor_number, 1);
            push_integer(-1);
          }
          for (k2 = i12 = 0; i12 < 1e4; k2 = ++i12) {
            try_kth_prime(k2);
            if (n_factor_number.compare(1) === 0) {
              return;
            }
          }
          return factor_b();
        };
        try_kth_prime = function(k2) {
          var count2, d3, q, r6;
          count2 = 0;
          d3 = mint(primetab[k2]);
          count2 = 0;
          while (1) {
            if (n_factor_number.compare(1) === 0) {
              if (count2) {
                push_factor(d3, count2);
              }
              return;
            }
            [q, r6] = mdivrem(n_factor_number, d3);
            if (r6.isZero()) {
              count2++;
              n_factor_number = q;
            } else {
              break;
            }
          }
          if (count2) {
            push_factor(d3, count2);
          }
          if (mcmp(q, d3) === -1) {
            push_factor(n_factor_number, 1);
            return n_factor_number = mint(1);
          }
        };
        factor_b = function() {
          var bigint_one, g2, k2, l8, t5, x2, xprime;
          k2 = 0;
          l8 = 0;
          bigint_one = mint(1);
          x2 = mint(5);
          xprime = mint(2);
          k2 = 1;
          l8 = 1;
          while (1) {
            if (mprime(n_factor_number)) {
              push_factor(n_factor_number, 1);
              return 0;
            }
            while (1) {
              if (esc_flag) {
                stop("esc");
              }
              t5 = msub(xprime, x2);
              t5 = setSignTo(t5, 1);
              g2 = mgcd(t5, n_factor_number);
              if (MEQUAL(g2, 1)) {
                if (--k2 === 0) {
                  xprime = x2;
                  l8 *= 2;
                  k2 = l8;
                }
                t5 = mmul(x2, x2);
                x2 = madd(t5, bigint_one);
                t5 = mmod(x2, n_factor_number);
                x2 = t5;
                continue;
              }
              push_factor(g2, 1);
              if (mcmp(g2, n_factor_number) === 0) {
                return -1;
              }
              t5 = mdiv(n_factor_number, g2);
              n_factor_number = t5;
              t5 = mmod(x2, n_factor_number);
              x2 = t5;
              t5 = mmod(xprime, n_factor_number);
              xprime = t5;
              break;
            }
          }
        };
        push_factor = function(d3, count2) {
          p1 = new U();
          p1.k = NUM;
          p1.q.a = d3;
          p1.q.b = mint(1);
          push(p1);
          if (count2 > 1) {
            push_symbol(POWER);
            swap();
            p1 = new U();
            p1.k = NUM;
            p1.q.a = mint(count2);
            p1.q.b = mint(1);
            push(p1);
            return list(3);
          }
        };
        DEBUG_POWER = false;
        Eval_power = function() {
          if (DEBUG_POWER) {
            debugger;
          }
          push(cadr(p1));
          Eval();
          push(caddr(p1));
          Eval();
          return power();
        };
        power = function() {
          save();
          yypower();
          return restore();
        };
        yypower = function() {
          var b_isEven_and_c_isItsInverse, hopefullySimplified, inputBase, inputExp, isThisOne, is_a_moreThanZero, n9;
          if (DEBUG_POWER) {
            debugger;
          }
          n9 = 0;
          p2 = pop();
          p1 = pop();
          inputExp = p2;
          inputBase = p1;
          if (DEBUG_POWER) {
            console.log("POWER: " + p1 + " ^ " + p2);
          }
          if (equal(p1, one) || isZeroAtomOrTensor(p2)) {
            if (evaluatingAsFloats) {
              push_double(1);
            } else {
              push(one);
            }
            if (DEBUG_POWER) {
              console.log("   power of " + inputBase + " ^ " + inputExp + ": " + stack[tos - 1]);
            }
            return;
          }
          if (equal(p2, one)) {
            push(p1);
            if (DEBUG_POWER) {
              console.log("   power of " + inputBase + " ^ " + inputExp + ": " + stack[tos - 1]);
            }
            return;
          }
          if (isminusone(p1) && isminusone(p2)) {
            if (evaluatingAsFloats) {
              push_double(1);
            } else {
              push(one);
            }
            negate();
            if (DEBUG_POWER) {
              console.log("   power of " + inputBase + " ^ " + inputExp + ": " + stack[tos - 1]);
            }
            return;
          }
          if (isminusone(p1) && isoneovertwo(p2)) {
            push(imaginaryunit);
            if (DEBUG_POWER) {
              console.log("   power of " + inputBase + " ^ " + inputExp + ": " + stack[tos - 1]);
            }
            return;
          }
          if (isminusone(p1) && isminusoneovertwo(p2)) {
            push(imaginaryunit);
            negate();
            if (DEBUG_POWER) {
              console.log("   power of " + inputBase + " ^ " + inputExp + ": " + stack[tos - 1]);
            }
            return;
          }
          if (isminusone(p1) && !isdouble(p1) && isrational(p2) && !isinteger(p2) && ispositivenumber(p2) && !evaluatingAsFloats) {
            if (DEBUG_POWER) {
              console.log("   power: -1 ^ rational");
            }
            if (DEBUG_POWER) {
              console.log(" trick: p2.q.a , p2.q.b " + p2.q.a + " , " + p2.q.b);
            }
            if (p2.q.a < p2.q.b) {
              push_symbol(POWER);
              push(p1);
              push(p2);
              list(3);
            } else {
              push_symbol(MULTIPLY);
              push(p1);
              push_symbol(POWER);
              push(p1);
              push_rational(p2.q.a.mod(p2.q.b), p2.q.b);
              list(3);
              list(3);
              if (DEBUG_POWER) {
                console.log(" trick applied : " + stack[tos - 1]);
              }
            }
            rect();
            if (DEBUG_POWER) {
              console.log("   power of " + inputBase + " ^ " + inputExp + ": " + stack[tos - 1]);
            }
            return;
          }
          if (isrational(p1) && isrational(p2)) {
            if (DEBUG_POWER) {
              console.log("   power: isrational(p1) && isrational(p2)");
            }
            push(p1);
            push(p2);
            qpow();
            if (DEBUG_POWER) {
              console.log("   power of " + inputBase + " ^ " + inputExp + ": " + stack[tos - 1]);
            }
            return;
          }
          if (isNumericAtom(p1) && isNumericAtom(p2)) {
            if (DEBUG_POWER) {
              console.log("   power: both base and exponent are either rational or double ");
            }
            if (DEBUG_POWER) {
              console.log("POWER - isNumericAtom(p1) && isNumericAtom(p2)");
            }
            push(p1);
            push(p2);
            dpow();
            if (DEBUG_POWER) {
              console.log("   power of " + inputBase + " ^ " + inputExp + ": " + stack[tos - 1]);
            }
            return;
          }
          if (istensor(p1)) {
            if (DEBUG_POWER) {
              console.log("   power: istensor(p1) ");
            }
            power_tensor();
            if (DEBUG_POWER) {
              console.log("   power of " + inputBase + " ^ " + inputExp + ": " + stack[tos - 1]);
            }
            return;
          }
          if (car(p1) === symbol(ABS) && iseveninteger(p2) && !isZeroAtomOrTensor(get_binding(symbol(ASSUME_REAL_VARIABLES)))) {
            if (DEBUG_POWER) {
              console.log("   power: even power of absolute of real value ");
            }
            push(cadr(p1));
            push(p2);
            power();
            if (DEBUG_POWER) {
              console.log("   power of " + inputBase + " ^ " + inputExp + ": " + stack[tos - 1]);
            }
            return;
          }
          if (p1 === symbol(E) && car(p2) === symbol(LOG)) {
            push(cadr(p2));
            if (DEBUG_POWER) {
              console.log("   power of " + inputBase + " ^ " + inputExp + ": " + stack[tos - 1]);
            }
            return;
          }
          if (p1 === symbol(E) && isdouble(p2)) {
            if (DEBUG_POWER) {
              console.log("   power: p1 == symbol(E) && isdouble(p2) ");
            }
            push_double(Math.exp(p2.d));
            if (DEBUG_POWER) {
              console.log("   power of " + inputBase + " ^ " + inputExp + ": " + stack[tos - 1]);
            }
            return;
          }
          if (p1 === symbol(E) && Find(p2, imaginaryunit) !== 0 && Find(p2, symbol(PI)) !== 0 && !evaluatingPolar) {
            push_symbol(POWER);
            push(p1);
            push(p2);
            list(3);
            if (DEBUG_POWER) {
              console.log("   power: turning complex exponential to rect: " + stack[tos - 1]);
            }
            rect();
            hopefullySimplified = pop();
            if (Find(hopefullySimplified, symbol(PI)) === 0) {
              if (DEBUG_POWER) {
                console.log("   power: turned complex exponential to rect: " + hopefullySimplified);
              }
              push(hopefullySimplified);
              return;
            }
          }
          if (car(p1) === symbol(MULTIPLY) && isinteger(p2)) {
            if (DEBUG_POWER) {
              console.log("   power: (a * b) ^ c  ->  (a ^ c) * (b ^ c) ");
            }
            p1 = cdr(p1);
            push(car(p1));
            push(p2);
            power();
            p1 = cdr(p1);
            while (iscons(p1)) {
              push(car(p1));
              push(p2);
              power();
              multiply();
              p1 = cdr(p1);
            }
            if (DEBUG_POWER) {
              console.log("   power of " + inputBase + " ^ " + inputExp + ": " + stack[tos - 1]);
            }
            return;
          }
          is_a_moreThanZero = false;
          if (isNumericAtom(cadr(p1))) {
            is_a_moreThanZero = sign(compare_numbers(cadr(p1), zero));
          }
          if (car(p1) === symbol(POWER) && (isinteger(p2) || is_a_moreThanZero)) {
            push(cadr(p1));
            push(caddr(p1));
            push(p2);
            multiply();
            power();
            if (DEBUG_POWER) {
              console.log("   power of " + inputBase + " ^ " + inputExp + ": " + stack[tos - 1]);
            }
            return;
          }
          b_isEven_and_c_isItsInverse = false;
          if (iseveninteger(caddr(p1))) {
            push(caddr(p1));
            push(p2);
            multiply();
            isThisOne = pop();
            if (isone(isThisOne)) {
              b_isEven_and_c_isItsInverse = true;
            }
          }
          if (car(p1) === symbol(POWER) && b_isEven_and_c_isItsInverse) {
            if (DEBUG_POWER) {
              console.log("   power: car(p1) == symbol(POWER) && b_isEven_and_c_isItsInverse ");
            }
            push(cadr(p1));
            abs();
            if (DEBUG_POWER) {
              console.log("   power of " + inputBase + " ^ " + inputExp + ": " + stack[tos - 1]);
            }
            return;
          }
          if (expanding && isadd(p1) && isNumericAtom(p2)) {
            push(p2);
            n9 = pop_integer();
            if (n9 > 1 && !isNaN(n9)) {
              if (DEBUG_POWER) {
                console.log("   power: expanding && isadd(p1) && isNumericAtom(p2) ");
              }
              power_sum(n9);
              if (DEBUG_POWER) {
                console.log("   power of " + inputBase + " ^ " + inputExp + ": " + stack[tos - 1]);
              }
              return;
            }
          }
          if (trigmode === 1 && car(p1) === symbol(SIN) && iseveninteger(p2)) {
            if (DEBUG_POWER) {
              console.log("   power: trigmode == 1 && car(p1) == symbol(SIN) && iseveninteger(p2) ");
            }
            push_integer(1);
            push(cadr(p1));
            cosine();
            push_integer(2);
            power();
            subtract();
            push(p2);
            push_rational(1, 2);
            multiply();
            power();
            if (DEBUG_POWER) {
              console.log("   power of " + inputBase + " ^ " + inputExp + ": " + stack[tos - 1]);
            }
            return;
          }
          if (trigmode === 2 && car(p1) === symbol(COS) && iseveninteger(p2)) {
            if (DEBUG_POWER) {
              console.log("   power: trigmode == 2 && car(p1) == symbol(COS) && iseveninteger(p2) ");
            }
            push_integer(1);
            push(cadr(p1));
            sine();
            push_integer(2);
            power();
            subtract();
            push(p2);
            push_rational(1, 2);
            multiply();
            power();
            if (DEBUG_POWER) {
              console.log("   power of " + inputBase + " ^ " + inputExp + ": " + stack[tos - 1]);
            }
            return;
          }
          if (iscomplexnumber(p1)) {
            if (DEBUG_POWER) {
              console.log(" power - handling the case (a + ib) ^ n");
            }
            if (isinteger(p2)) {
              push(p1);
              conjugate();
              p3 = pop();
              push(p3);
              push(p3);
              push(p1);
              multiply();
              divide();
              if (!isone(p2)) {
                push(p2);
                negate();
                power();
              }
              if (DEBUG_POWER) {
                console.log("   power of " + inputBase + " ^ " + inputExp + ": " + stack[tos - 1]);
              }
              return;
            }
            if (isNumericAtom(p2)) {
              push(p1);
              abs();
              push(p2);
              power();
              push_integer(-1);
              push(p1);
              arg();
              push(p2);
              multiply();
              if (evaluatingAsFloats || iscomplexnumberdouble(p1) && isdouble(p2)) {
                push_double(Math.PI);
              } else {
                push(symbol(PI));
              }
              divide();
              power();
              multiply();
              if (avoidCalculatingPowersIntoArctans) {
                if (Find(stack[tos - 1], symbol(ARCTAN))) {
                  pop();
                  push_symbol(POWER);
                  push(p1);
                  push(p2);
                  list(3);
                }
              }
              if (DEBUG_POWER) {
                console.log("   power of " + inputBase + " ^ " + inputExp + ": " + stack[tos - 1]);
              }
              return;
            }
          }
          if (simplify_polar()) {
            if (DEBUG_POWER) {
              console.log("   power: using simplify_polar");
            }
            return;
          }
          if (DEBUG_POWER) {
            console.log("   power: nothing can be done ");
          }
          push_symbol(POWER);
          push(p1);
          push(p2);
          list(3);
          if (DEBUG_POWER) {
            return console.log("   power of " + inputBase + " ^ " + inputExp + ": " + stack[tos - 1]);
          }
        };
        power_sum = function(n9) {
          var a4, i5, i12, j2, j12, k2, l1, ref12, ref2, ref3;
          a4 = [];
          i5 = 0;
          j2 = 0;
          k2 = 0;
          k2 = length(p1) - 1;
          push_frame(k2 * (n9 + 1));
          p1 = cdr(p1);
          for (i5 = i12 = 0, ref12 = k2; 0 <= ref12 ? i12 < ref12 : i12 > ref12; i5 = 0 <= ref12 ? ++i12 : --i12) {
            for (j2 = j12 = 0, ref2 = n9; 0 <= ref2 ? j12 <= ref2 : j12 >= ref2; j2 = 0 <= ref2 ? ++j12 : --j12) {
              push(car(p1));
              push_integer(j2);
              power();
              stack[frame + i5 * (n9 + 1) + j2] = pop();
            }
            p1 = cdr(p1);
          }
          push_integer(n9);
          factorial();
          p1 = pop();
          for (i5 = l1 = 0, ref3 = k2; 0 <= ref3 ? l1 < ref3 : l1 > ref3; i5 = 0 <= ref3 ? ++l1 : --l1) {
            a4[i5] = 0;
          }
          push(zero);
          multinomial_sum(k2, n9, a4, 0, n9);
          return pop_frame(k2 * (n9 + 1));
        };
        multinomial_sum = function(k2, n9, a4, i5, m3) {
          var i12, j2, j12, l1, ref12, ref2, ref3;
          j2 = 0;
          if (i5 < k2 - 1) {
            for (j2 = i12 = 0, ref12 = m3; 0 <= ref12 ? i12 <= ref12 : i12 >= ref12; j2 = 0 <= ref12 ? ++i12 : --i12) {
              a4[i5] = j2;
              multinomial_sum(k2, n9, a4, i5 + 1, m3 - j2);
            }
            return;
          }
          a4[i5] = m3;
          push(p1);
          for (j2 = j12 = 0, ref2 = k2; 0 <= ref2 ? j12 < ref2 : j12 > ref2; j2 = 0 <= ref2 ? ++j12 : --j12) {
            push_integer(a4[j2]);
            factorial();
            divide();
          }
          for (j2 = l1 = 0, ref3 = k2; 0 <= ref3 ? l1 < ref3 : l1 > ref3; j2 = 0 <= ref3 ? ++l1 : --l1) {
            push(stack[frame + j2 * (n9 + 1) + a4[j2]]);
            multiply();
          }
          return add();
        };
        simplify_polar = function() {
          var doNothing, n9;
          n9 = 0;
          n9 = isquarterturn(p2);
          switch (n9) {
            case 0:
              doNothing = 1;
              break;
            case 1:
              push_integer(1);
              return 1;
            case 2:
              push_integer(-1);
              return 1;
            case 3:
              push(imaginaryunit);
              return 1;
            case 4:
              push(imaginaryunit);
              negate();
              return 1;
          }
          if (car(p2) === symbol(ADD)) {
            p3 = cdr(p2);
            while (iscons(p3)) {
              n9 = isquarterturn(car(p3));
              if (n9) {
                break;
              }
              p3 = cdr(p3);
            }
            switch (n9) {
              case 0:
                return 0;
              case 1:
                push_integer(1);
                break;
              case 2:
                push_integer(-1);
                break;
              case 3:
                push(imaginaryunit);
                break;
              case 4:
                push(imaginaryunit);
                negate();
            }
            push(p2);
            push(car(p3));
            subtract();
            exponential();
            multiply();
            return 1;
          }
          return 0;
        };
        Eval_prime = function() {
          push(cadr(p1));
          Eval();
          return prime();
        };
        prime = function() {
          var n9;
          n9 = 0;
          n9 = pop_integer();
          if (n9 < 1 || n9 > MAXPRIMETAB) {
            stop("prime: Argument out of range.");
          }
          n9 = primetab[n9 - 1];
          return push_integer(n9);
        };
        power_str = "^";
        codeGen = false;
        Eval_print = function() {
          stringsEmittedByUserPrintouts += _print(cdr(p1), printMode);
          return push(symbol(NIL));
        };
        Eval_print2dascii = function() {
          stringsEmittedByUserPrintouts += _print(cdr(p1), PRINTMODE_2DASCII);
          return push(symbol(NIL));
        };
        Eval_printcomputer = function() {
          stringsEmittedByUserPrintouts += _print(cdr(p1), PRINTMODE_COMPUTER);
          return push(symbol(NIL));
        };
        Eval_printlatex = function() {
          stringsEmittedByUserPrintouts += _print(cdr(p1), PRINTMODE_LATEX);
          return push(symbol(NIL));
        };
        Eval_printhuman = function() {
          var original_test_flag;
          original_test_flag = test_flag;
          test_flag = 0;
          stringsEmittedByUserPrintouts += _print(cdr(p1), PRINTMODE_HUMAN);
          test_flag = original_test_flag;
          return push(symbol(NIL));
        };
        Eval_printlist = function() {
          var beenPrinted;
          beenPrinted = _print(cdr(p1), PRINTMODE_LIST);
          stringsEmittedByUserPrintouts += beenPrinted;
          return push(symbol(NIL));
        };
        _print = function(p11, passedPrintMode) {
          var accumulator, origPrintMode;
          accumulator = "";
          while (iscons(p11)) {
            push(car(p11));
            Eval();
            p2 = pop();
            origPrintMode = printMode;
            if (passedPrintMode === PRINTMODE_COMPUTER) {
              printMode = PRINTMODE_COMPUTER;
              accumulator = printline(p2);
              rememberPrint(accumulator, LAST_FULL_PRINT);
            } else if (passedPrintMode === PRINTMODE_HUMAN) {
              printMode = PRINTMODE_HUMAN;
              accumulator = printline(p2);
              rememberPrint(accumulator, LAST_PLAIN_PRINT);
            } else if (passedPrintMode === PRINTMODE_2DASCII) {
              printMode = PRINTMODE_2DASCII;
              accumulator = print2dascii(p2);
              rememberPrint(accumulator, LAST_2DASCII_PRINT);
            } else if (passedPrintMode === PRINTMODE_LATEX) {
              printMode = PRINTMODE_LATEX;
              accumulator = printline(p2);
              rememberPrint(accumulator, LAST_LATEX_PRINT);
            } else if (passedPrintMode === PRINTMODE_LIST) {
              printMode = PRINTMODE_LIST;
              accumulator = print_list(p2);
              rememberPrint(accumulator, LAST_LIST_PRINT);
            }
            printMode = origPrintMode;
            p11 = cdr(p11);
          }
          if (DEBUG) {
            console.log("emttedString from display: " + stringsEmittedByUserPrintouts);
          }
          return accumulator;
        };
        rememberPrint = function(theString, theTypeOfPrint) {
          var parsedString;
          scan('"' + theString + '"');
          parsedString = pop();
          return set_binding(symbol(theTypeOfPrint), parsedString);
        };
        print_str = function(s7) {
          if (DEBUG) {
            console.log("emttedString from print_str: " + stringsEmittedByUserPrintouts);
          }
          return s7;
        };
        print_char = function(c6) {
          return c6;
        };
        collectLatexStringFromReturnValue = function(p11) {
          var origPrintMode, originalCodeGen, returnedString;
          origPrintMode = printMode;
          printMode = PRINTMODE_LATEX;
          originalCodeGen = codeGen;
          codeGen = false;
          returnedString = print_expr(p11);
          returnedString = returnedString.replace(/_/g, "\\_");
          printMode = origPrintMode;
          codeGen = originalCodeGen;
          if (DEBUG) {
            console.log("emttedString from collectLatexStringFromReturnValue: " + stringsEmittedByUserPrintouts);
          }
          return returnedString;
        };
        printline = function(p11) {
          var accumulator;
          accumulator = "";
          accumulator += print_expr(p11);
          return accumulator;
        };
        print_base_of_denom = function(p12) {
          var accumulator;
          accumulator = "";
          if (isfraction(p12) || car(p12) === symbol(ADD) || car(p12) === symbol(MULTIPLY) || car(p12) === symbol(POWER) || lessp(p12, zero)) {
            accumulator += print_char("(");
            accumulator += print_expr(p12);
            accumulator += print_char(")");
          } else {
            accumulator += print_expr(p12);
          }
          return accumulator;
        };
        print_expo_of_denom = function(p22) {
          var accumulator;
          accumulator = "";
          if (isfraction(p22) || car(p22) === symbol(ADD) || car(p22) === symbol(MULTIPLY) || car(p22) === symbol(POWER)) {
            accumulator += print_char("(");
            accumulator += print_expr(p22);
            accumulator += print_char(")");
          } else {
            accumulator += print_expr(p22);
          }
          return accumulator;
        };
        print_denom = function(p11, d3) {
          var accumulator;
          accumulator = "";
          save();
          p1 = cadr(p11);
          p2 = caddr(p11);
          if (isminusone(p2)) {
            accumulator += print_base_of_denom(p1);
            restore();
            return accumulator;
          }
          if (d3 === 1) {
            accumulator += print_char("(");
          }
          push(p2);
          negate();
          p2 = pop();
          accumulator += print_power(p1, p2);
          if (d3 === 1) {
            accumulator += print_char(")");
          }
          restore();
          return accumulator;
        };
        print_a_over_b = function(p11) {
          var accumulator, d3, doNothing, n9;
          accumulator = "";
          flag = 0;
          n9 = 0;
          d3 = 0;
          save();
          n9 = 0;
          d3 = 0;
          p1 = cdr(p11);
          p2 = car(p1);
          if (isrational(p2)) {
            push(p2);
            mp_numerator();
            absval();
            p3 = pop();
            push(p2);
            mp_denominator();
            p4 = pop();
            if (!isplusone(p3)) {
              n9++;
            }
            if (!isplusone(p4)) {
              d3++;
            }
            p1 = cdr(p1);
          } else {
            p3 = one;
            p4 = one;
          }
          while (iscons(p1)) {
            p2 = car(p1);
            if (is_denominator(p2)) {
              d3++;
            } else {
              n9++;
            }
            p1 = cdr(p1);
          }
          if (printMode === PRINTMODE_LATEX) {
            accumulator += print_str("\\frac{");
          }
          if (n9 === 0) {
            accumulator += print_char("1");
          } else {
            flag = 0;
            p1 = cdr(p11);
            if (isrational(car(p1))) {
              p1 = cdr(p1);
            }
            if (!isplusone(p3)) {
              accumulator += print_factor(p3);
              flag = 1;
            }
            while (iscons(p1)) {
              p2 = car(p1);
              if (is_denominator(p2)) {
                doNothing = 1;
              } else {
                if (flag) {
                  accumulator += print_multiply_sign();
                }
                accumulator += print_factor(p2);
                flag = 1;
              }
              p1 = cdr(p1);
            }
          }
          if (printMode === PRINTMODE_LATEX) {
            accumulator += print_str("}{");
          } else if (printMode === PRINTMODE_HUMAN && !test_flag) {
            accumulator += print_str(" / ");
          } else {
            accumulator += print_str("/");
          }
          if (d3 > 1 && printMode !== PRINTMODE_LATEX) {
            accumulator += print_char("(");
          }
          flag = 0;
          p1 = cdr(p11);
          if (isrational(car(p1))) {
            p1 = cdr(p1);
          }
          if (!isplusone(p4)) {
            accumulator += print_factor(p4);
            flag = 1;
          }
          while (iscons(p1)) {
            p2 = car(p1);
            if (is_denominator(p2)) {
              if (flag) {
                accumulator += print_multiply_sign();
              }
              accumulator += print_denom(p2, d3);
              flag = 1;
            }
            p1 = cdr(p1);
          }
          if (d3 > 1 && printMode !== PRINTMODE_LATEX) {
            accumulator += print_char(")");
          }
          if (printMode === PRINTMODE_LATEX) {
            accumulator += print_str("}");
          }
          restore();
          return accumulator;
        };
        print_expr = function(p11) {
          var accumulator;
          accumulator = "";
          if (isadd(p11)) {
            p11 = cdr(p11);
            if (sign_of_term(car(p11)) === "-") {
              accumulator += print_str("-");
            }
            accumulator += print_term(car(p11));
            p11 = cdr(p11);
            while (iscons(p11)) {
              if (sign_of_term(car(p11)) === "+") {
                if (printMode === PRINTMODE_HUMAN && !test_flag) {
                  accumulator += print_str(" + ");
                } else {
                  accumulator += print_str("+");
                }
              } else {
                if (printMode === PRINTMODE_HUMAN && !test_flag) {
                  accumulator += print_str(" - ");
                } else {
                  accumulator += print_str("-");
                }
              }
              accumulator += print_term(car(p11));
              p11 = cdr(p11);
            }
          } else {
            if (sign_of_term(p11) === "-") {
              accumulator += print_str("-");
            }
            accumulator += print_term(p11);
          }
          return accumulator;
        };
        sign_of_term = function(p11) {
          var accumulator;
          accumulator = "";
          if (car(p11) === symbol(MULTIPLY) && isNumericAtom(cadr(p11)) && lessp(cadr(p11), zero)) {
            accumulator += "-";
          } else if (isNumericAtom(p11) && lessp(p11, zero)) {
            accumulator += "-";
          } else {
            accumulator += "+";
          }
          return accumulator;
        };
        print_term = function(p11) {
          var accumulator, denom, numberOneOverSomething, origAccumulator, previousFactorWasANumber;
          accumulator = "";
          if (car(p11) === symbol(MULTIPLY) && any_denominators(p11)) {
            accumulator += print_a_over_b(p11);
            return accumulator;
          }
          if (car(p11) === symbol(MULTIPLY)) {
            p11 = cdr(p11);
            if (isminusone(car(p11))) {
              p11 = cdr(p11);
            }
            previousFactorWasANumber = false;
            if (isNumericAtom(car(p11))) {
              previousFactorWasANumber = true;
            }
            numberOneOverSomething = false;
            if (printMode === PRINTMODE_LATEX && iscons(cdr(p11)) && isNumberOneOverSomething(car(p11))) {
              numberOneOverSomething = true;
              denom = car(p11).q.b.toString();
            }
            if (numberOneOverSomething) {
              origAccumulator = accumulator;
              accumulator = "";
            } else {
              accumulator += print_factor(car(p11));
            }
            p11 = cdr(p11);
            while (iscons(p11)) {
              if (printMode === PRINTMODE_LATEX) {
                if (previousFactorWasANumber) {
                  if (caar(p11) === symbol(POWER)) {
                    if (isNumericAtom(car(cdr(car(p11))))) {
                      if (!isfraction(car(cdr(cdr(car(p11)))))) {
                        accumulator += " \\cdot ";
                      }
                    }
                  }
                }
              }
              accumulator += print_multiply_sign();
              accumulator += print_factor(car(p11), false, true);
              previousFactorWasANumber = false;
              if (isNumericAtom(car(p11))) {
                previousFactorWasANumber = true;
              }
              p11 = cdr(p11);
            }
            if (numberOneOverSomething) {
              accumulator = origAccumulator + "\\frac{" + accumulator + "}{" + denom + "}";
            }
          } else {
            accumulator += print_factor(p11);
          }
          return accumulator;
        };
        print_subexpr = function(p11) {
          var accumulator;
          accumulator = "";
          accumulator += print_char("(");
          accumulator += print_expr(p11);
          accumulator += print_char(")");
          return accumulator;
        };
        print_factorial_function = function(p11) {
          var accumulator;
          accumulator = "";
          p11 = cadr(p11);
          if (isfraction(p11) || car(p11) === symbol(ADD) || car(p11) === symbol(MULTIPLY) || car(p11) === symbol(POWER) || car(p11) === symbol(FACTORIAL)) {
            accumulator += print_subexpr(p11);
          } else {
            accumulator += print_expr(p11);
          }
          accumulator += print_char("!");
          return accumulator;
        };
        print_ABS_latex = function(p11) {
          var accumulator;
          accumulator = "";
          accumulator += print_str("\\left |");
          accumulator += print_expr(cadr(p11));
          accumulator += print_str(" \\right |");
          return accumulator;
        };
        print_BINOMIAL_latex = function(p11) {
          var accumulator;
          accumulator = "";
          accumulator += print_str("\\binom{");
          accumulator += print_expr(cadr(p11));
          accumulator += print_str("}{");
          accumulator += print_expr(caddr(p11));
          accumulator += print_str("} ");
          return accumulator;
        };
        print_DOT_latex = function(p11) {
          var accumulator;
          accumulator = "";
          accumulator += print_expr(cadr(p11));
          accumulator += print_str(" \\cdot ");
          accumulator += print_expr(caddr(p11));
          return accumulator;
        };
        print_DOT_codegen = function(p11) {
          var accumulator;
          accumulator = "dot(";
          accumulator += print_expr(cadr(p11));
          accumulator += ", ";
          accumulator += print_expr(caddr(p11));
          accumulator += ")";
          return accumulator;
        };
        print_SIN_codegen = function(p11) {
          var accumulator;
          accumulator = "Math.sin(";
          accumulator += print_expr(cadr(p11));
          accumulator += ")";
          return accumulator;
        };
        print_COS_codegen = function(p11) {
          var accumulator;
          accumulator = "Math.cos(";
          accumulator += print_expr(cadr(p11));
          accumulator += ")";
          return accumulator;
        };
        print_TAN_codegen = function(p11) {
          var accumulator;
          accumulator = "Math.tan(";
          accumulator += print_expr(cadr(p11));
          accumulator += ")";
          return accumulator;
        };
        print_ARCSIN_codegen = function(p11) {
          var accumulator;
          accumulator = "Math.asin(";
          accumulator += print_expr(cadr(p11));
          accumulator += ")";
          return accumulator;
        };
        print_ARCCOS_codegen = function(p11) {
          var accumulator;
          accumulator = "Math.acos(";
          accumulator += print_expr(cadr(p11));
          accumulator += ")";
          return accumulator;
        };
        print_ARCTAN_codegen = function(p11) {
          var accumulator;
          accumulator = "Math.atan(";
          accumulator += print_expr(cadr(p11));
          accumulator += ")";
          return accumulator;
        };
        print_SQRT_latex = function(p11) {
          var accumulator;
          accumulator = "";
          accumulator += print_str("\\sqrt{");
          accumulator += print_expr(cadr(p11));
          accumulator += print_str("} ");
          return accumulator;
        };
        print_TRANSPOSE_latex = function(p11) {
          var accumulator;
          accumulator = "";
          accumulator += print_str("{");
          if (iscons(cadr(p11))) {
            accumulator += print_str("(");
          }
          accumulator += print_expr(cadr(p11));
          if (iscons(cadr(p11))) {
            accumulator += print_str(")");
          }
          accumulator += print_str("}");
          accumulator += print_str("^T");
          return accumulator;
        };
        print_TRANSPOSE_codegen = function(p11) {
          var accumulator;
          accumulator = "";
          accumulator += print_str("transpose(");
          accumulator += print_expr(cadr(p11));
          accumulator += print_str(")");
          return accumulator;
        };
        print_UNIT_codegen = function(p11) {
          var accumulator;
          accumulator = "";
          accumulator += print_str("identity(");
          accumulator += print_expr(cadr(p11));
          accumulator += print_str(")");
          return accumulator;
        };
        print_INV_latex = function(p11) {
          var accumulator;
          accumulator = "";
          accumulator += print_str("{");
          if (iscons(cadr(p11))) {
            accumulator += print_str("(");
          }
          accumulator += print_expr(cadr(p11));
          if (iscons(cadr(p11))) {
            accumulator += print_str(")");
          }
          accumulator += print_str("}");
          accumulator += print_str("^{-1}");
          return accumulator;
        };
        print_INV_codegen = function(p11) {
          var accumulator;
          accumulator = "";
          accumulator += print_str("inv(");
          accumulator += print_expr(cadr(p11));
          accumulator += print_str(")");
          return accumulator;
        };
        print_DEFINT_latex = function(p11) {
          var accumulator, functionBody, i5, i12, numberOfIntegrals, originalIntegral, ref12, theIntegral, theVariable;
          accumulator = "";
          functionBody = car(cdr(p11));
          p11 = cdr(p11);
          originalIntegral = p11;
          numberOfIntegrals = 0;
          while (iscons(cdr(cdr(p11)))) {
            numberOfIntegrals++;
            theIntegral = cdr(cdr(p11));
            accumulator += print_str("\\int^{");
            accumulator += print_expr(car(cdr(theIntegral)));
            accumulator += print_str("}_{");
            accumulator += print_expr(car(theIntegral));
            accumulator += print_str("} \\! ");
            p11 = cdr(theIntegral);
          }
          accumulator += print_expr(functionBody);
          accumulator += print_str(" \\,");
          p11 = originalIntegral;
          for (i5 = i12 = 1, ref12 = numberOfIntegrals; 1 <= ref12 ? i12 <= ref12 : i12 >= ref12; i5 = 1 <= ref12 ? ++i12 : --i12) {
            theVariable = cdr(p11);
            accumulator += print_str(" \\mathrm{d} ");
            accumulator += print_expr(car(theVariable));
            if (i5 < numberOfIntegrals) {
              accumulator += print_str(" \\, ");
            }
            p11 = cdr(cdr(theVariable));
          }
          return accumulator;
        };
        print_tensor = function(p11) {
          var accumulator;
          accumulator = "";
          accumulator += print_tensor_inner(p11, 0, 0)[1];
          return accumulator;
        };
        print_tensor_inner = function(p11, j2, k2) {
          var accumulator, i5, i12, j12, ref12, ref2, retString;
          accumulator = "";
          accumulator += print_str("[");
          if (j2 < p11.tensor.ndim - 1) {
            for (i5 = i12 = 0, ref12 = p11.tensor.dim[j2]; 0 <= ref12 ? i12 < ref12 : i12 > ref12; i5 = 0 <= ref12 ? ++i12 : --i12) {
              [k2, retString] = print_tensor_inner(p11, j2 + 1, k2);
              accumulator += retString;
              if (i5 !== p11.tensor.dim[j2] - 1) {
                accumulator += print_str(",");
              }
            }
          } else {
            for (i5 = j12 = 0, ref2 = p11.tensor.dim[j2]; 0 <= ref2 ? j12 < ref2 : j12 > ref2; i5 = 0 <= ref2 ? ++j12 : --j12) {
              accumulator += print_expr(p11.tensor.elem[k2]);
              if (i5 !== p11.tensor.dim[j2] - 1) {
                accumulator += print_str(",");
              }
              k2++;
            }
          }
          accumulator += print_str("]");
          return [k2, accumulator];
        };
        print_tensor_latex = function(p11) {
          var accumulator;
          accumulator = "";
          if (p11.tensor.ndim <= 2) {
            accumulator += print_tensor_inner_latex(true, p11, 0, 0)[1];
          }
          return accumulator;
        };
        print_tensor_inner_latex = function(firstLevel, p11, j2, k2) {
          var accumulator, i5, i12, j12, ref12, ref2, retString;
          accumulator = "";
          if (firstLevel) {
            accumulator += "\\begin{bmatrix} ";
          }
          if (j2 < p11.tensor.ndim - 1) {
            for (i5 = i12 = 0, ref12 = p11.tensor.dim[j2]; 0 <= ref12 ? i12 < ref12 : i12 > ref12; i5 = 0 <= ref12 ? ++i12 : --i12) {
              [k2, retString] = print_tensor_inner_latex(0, p11, j2 + 1, k2);
              accumulator += retString;
              if (i5 !== p11.tensor.dim[j2] - 1) {
                accumulator += print_str(" \\\\ ");
              }
            }
          } else {
            for (i5 = j12 = 0, ref2 = p11.tensor.dim[j2]; 0 <= ref2 ? j12 < ref2 : j12 > ref2; i5 = 0 <= ref2 ? ++j12 : --j12) {
              accumulator += print_expr(p11.tensor.elem[k2]);
              if (i5 !== p11.tensor.dim[j2] - 1) {
                accumulator += print_str(" & ");
              }
              k2++;
            }
          }
          if (firstLevel) {
            accumulator += " \\end{bmatrix}";
          }
          return [k2, accumulator];
        };
        print_SUM_latex = function(p11) {
          var accumulator;
          accumulator = "\\sum_{";
          accumulator += print_expr(caddr(p11));
          accumulator += "=";
          accumulator += print_expr(cadddr(p11));
          accumulator += "}^{";
          accumulator += print_expr(caddddr(p11));
          accumulator += "}{";
          accumulator += print_expr(cadr(p11));
          accumulator += "}";
          return accumulator;
        };
        print_SUM_codegen = function(p11) {
          var accumulator, body, lowerlimit, upperlimit, variable;
          body = cadr(p11);
          variable = caddr(p11);
          lowerlimit = cadddr(p11);
          upperlimit = caddddr(p11);
          accumulator = "(function(){ var " + variable + ";  var holderSum = 0;  var lowerlimit = " + print_expr(lowerlimit) + ";  var upperlimit = " + print_expr(upperlimit) + ";  for (" + variable + " = lowerlimit; " + variable + " < upperlimit; " + variable + "++) {    holderSum += " + print_expr(body) + "; }  return holderSum;})()";
          return accumulator;
        };
        print_TEST_latex = function(p11) {
          var accumulator;
          accumulator = "\\left\\{ \\begin{array}{ll}";
          p11 = cdr(p11);
          while (iscons(p11)) {
            if (cdr(p11) === symbol(NIL)) {
              accumulator += "{";
              accumulator += print_expr(car(p11));
              accumulator += "} & otherwise ";
              accumulator += " \\\\\\\\";
              break;
            }
            accumulator += "{";
            accumulator += print_expr(cadr(p11));
            accumulator += "} & if & ";
            accumulator += print_expr(car(p11));
            accumulator += " \\\\\\\\";
            p11 = cddr(p11);
          }
          accumulator = accumulator.substring(0, accumulator.length - 4);
          return accumulator += "\\end{array} \\right.";
        };
        print_TEST_codegen = function(p11) {
          var accumulator, howManyIfs;
          accumulator = "(function(){";
          p11 = cdr(p11);
          howManyIfs = 0;
          while (iscons(p11)) {
            if (cdr(p11) === symbol(NIL)) {
              accumulator += "else {";
              accumulator += "return (" + print_expr(car(p11)) + ");";
              accumulator += "}";
              break;
            }
            if (howManyIfs) {
              accumulator += " else ";
            }
            accumulator += "if (" + print_expr(car(p11)) + "){";
            accumulator += "return (" + print_expr(cadr(p11)) + ");";
            accumulator += "}";
            howManyIfs++;
            p11 = cddr(p11);
          }
          accumulator += "})()";
          return accumulator;
        };
        print_TESTLT_latex = function(p11) {
          var accumulator;
          accumulator = "{";
          accumulator += print_expr(cadr(p11));
          accumulator += "}";
          accumulator += " < ";
          accumulator += "{";
          accumulator += print_expr(caddr(p11));
          return accumulator += "}";
        };
        print_TESTLE_latex = function(p11) {
          var accumulator;
          accumulator = "{";
          accumulator += print_expr(cadr(p11));
          accumulator += "}";
          accumulator += " \\leq ";
          accumulator += "{";
          accumulator += print_expr(caddr(p11));
          return accumulator += "}";
        };
        print_TESTGT_latex = function(p11) {
          var accumulator;
          accumulator = "{";
          accumulator += print_expr(cadr(p11));
          accumulator += "}";
          accumulator += " > ";
          accumulator += "{";
          accumulator += print_expr(caddr(p11));
          return accumulator += "}";
        };
        print_TESTGE_latex = function(p11) {
          var accumulator;
          accumulator = "{";
          accumulator += print_expr(cadr(p11));
          accumulator += "}";
          accumulator += " \\geq ";
          accumulator += "{";
          accumulator += print_expr(caddr(p11));
          return accumulator += "}";
        };
        print_TESTEQ_latex = function(p11) {
          var accumulator;
          accumulator = "{";
          accumulator += print_expr(cadr(p11));
          accumulator += "}";
          accumulator += " = ";
          accumulator += "{";
          accumulator += print_expr(caddr(p11));
          return accumulator += "}";
        };
        print_FOR_codegen = function(p11) {
          var accumulator, body, lowerlimit, upperlimit, variable;
          body = cadr(p11);
          variable = caddr(p11);
          lowerlimit = cadddr(p11);
          upperlimit = caddddr(p11);
          accumulator = "(function(){ var " + variable + ";  var lowerlimit = " + print_expr(lowerlimit) + ";  var upperlimit = " + print_expr(upperlimit) + ";  for (" + variable + " = lowerlimit; " + variable + " < upperlimit; " + variable + "++) {    " + print_expr(body) + " } })()";
          return accumulator;
        };
        print_DO_codegen = function(p11) {
          var accumulator;
          accumulator = "";
          p11 = cdr(p11);
          while (iscons(p11)) {
            accumulator += print_expr(car(p11));
            p11 = cdr(p11);
          }
          return accumulator;
        };
        print_SETQ_codegen = function(p11) {
          var accumulator;
          accumulator = "";
          accumulator += print_expr(cadr(p11));
          accumulator += " = ";
          accumulator += print_expr(caddr(p11));
          accumulator += "; ";
          return accumulator;
        };
        print_PRODUCT_latex = function(p11) {
          var accumulator;
          accumulator = "\\prod_{";
          accumulator += print_expr(caddr(p11));
          accumulator += "=";
          accumulator += print_expr(cadddr(p11));
          accumulator += "}^{";
          accumulator += print_expr(caddddr(p11));
          accumulator += "}{";
          accumulator += print_expr(cadr(p11));
          accumulator += "}";
          return accumulator;
        };
        print_PRODUCT_codegen = function(p11) {
          var accumulator, body, lowerlimit, upperlimit, variable;
          body = cadr(p11);
          variable = caddr(p11);
          lowerlimit = cadddr(p11);
          upperlimit = caddddr(p11);
          accumulator = "(function(){ var " + variable + ";  var holderProduct = 1;  var lowerlimit = " + print_expr(lowerlimit) + ";  var upperlimit = " + print_expr(upperlimit) + ";  for (" + variable + " = lowerlimit; " + variable + " < upperlimit; " + variable + "++) {    holderProduct *= " + print_expr(body) + "; }  return holderProduct;})()";
          return accumulator;
        };
        print_base = function(p11) {
          var accumulator;
          accumulator = "";
          if (isadd(cadr(p11)) || caadr(p11) === symbol(MULTIPLY) || caadr(p11) === symbol(POWER) || isnegativenumber(cadr(p11))) {
            accumulator += print_str("(");
            accumulator += print_expr(cadr(p11));
            accumulator += print_str(")");
          } else if (isNumericAtom(cadr(p11)) && (lessp(cadr(p11), zero) || isfraction(cadr(p11)))) {
            accumulator += print_str("(");
            accumulator += print_factor(cadr(p11));
            accumulator += print_str(")");
          } else {
            accumulator += print_factor(cadr(p11));
          }
          return accumulator;
        };
        print_exponent = function(p11) {
          var accumulator;
          accumulator = "";
          if (iscons(caddr(p11)) || isfraction(caddr(p11)) || isNumericAtom(caddr(p11)) && lessp(caddr(p11), zero)) {
            accumulator += print_str("(");
            accumulator += print_expr(caddr(p11));
            accumulator += print_str(")");
          } else {
            accumulator += print_factor(caddr(p11));
          }
          return accumulator;
        };
        print_power = function(base, exponent) {
          var accumulator, denomExponent, newExponent, numExponent;
          accumulator = "";
          if (DEBUG) {
            console.log("power base: " + base + "  exponent: " + exponent);
          }
          if (isoneovertwo(exponent)) {
            if (equaln(base, 2)) {
              if (codeGen) {
                accumulator += print_str("Math.SQRT2");
                return accumulator;
              }
            } else {
              if (printMode === PRINTMODE_LATEX) {
                accumulator += print_str("\\sqrt{");
                accumulator += print_expr(base);
                accumulator += print_str("}");
                return accumulator;
              } else if (codeGen) {
                accumulator += print_str("Math.sqrt(");
                accumulator += print_expr(base);
                accumulator += print_str(")");
                return accumulator;
              }
            }
          }
          if (equaln(get_binding(symbol(PRINT_LEAVE_E_ALONE)), 1) && base === symbol(E)) {
            if (codeGen) {
              accumulator += print_str("Math.exp(");
              accumulator += print_expo_of_denom(exponent);
              accumulator += print_str(")");
              return accumulator;
            }
            if (printMode === PRINTMODE_LATEX) {
              accumulator += print_str("e^{");
              accumulator += print_expr(exponent);
              accumulator += print_str("}");
            } else {
              accumulator += print_str("exp(");
              accumulator += print_expr(exponent);
              accumulator += print_str(")");
            }
            return accumulator;
          }
          if (codeGen) {
            accumulator += print_str("Math.pow(");
            accumulator += print_base_of_denom(base);
            accumulator += print_str(", ");
            accumulator += print_expo_of_denom(exponent);
            accumulator += print_str(")");
            return accumulator;
          }
          if (equaln(get_binding(symbol(PRINT_LEAVE_X_ALONE)), 0) || base.printname !== "x") {
            if (base !== symbol(E)) {
              if (isminusone(exponent)) {
                if (printMode === PRINTMODE_LATEX) {
                  accumulator += print_str("\\frac{1}{");
                } else if (printMode === PRINTMODE_HUMAN && !test_flag) {
                  accumulator += print_str("1 / ");
                } else {
                  accumulator += print_str("1/");
                }
                if (iscons(base) && printMode !== PRINTMODE_LATEX) {
                  accumulator += print_str("(");
                  accumulator += print_expr(base);
                  accumulator += print_str(")");
                } else {
                  accumulator += print_expr(base);
                }
                if (printMode === PRINTMODE_LATEX) {
                  accumulator += print_str("}");
                }
                return accumulator;
              }
              if (isnegativeterm(exponent)) {
                if (printMode === PRINTMODE_LATEX) {
                  accumulator += print_str("\\frac{1}{");
                } else if (printMode === PRINTMODE_HUMAN && !test_flag) {
                  accumulator += print_str("1 / ");
                } else {
                  accumulator += print_str("1/");
                }
                push(exponent);
                push_integer(-1);
                multiply();
                newExponent = pop();
                if (iscons(base) && printMode !== PRINTMODE_LATEX) {
                  accumulator += print_str("(");
                  accumulator += print_power(base, newExponent);
                  accumulator += print_str(")");
                } else {
                  accumulator += print_power(base, newExponent);
                }
                if (printMode === PRINTMODE_LATEX) {
                  accumulator += print_str("}");
                }
                return accumulator;
              }
            }
            if (isfraction(exponent) && printMode === PRINTMODE_LATEX) {
              accumulator += print_str("\\sqrt");
              push(exponent);
              denominator();
              denomExponent = pop();
              if (!isplustwo(denomExponent)) {
                accumulator += print_str("[");
                accumulator += print_expr(denomExponent);
                accumulator += print_str("]");
              }
              accumulator += print_str("{");
              push(exponent);
              numerator();
              numExponent = pop();
              exponent = numExponent;
              accumulator += print_power(base, exponent);
              accumulator += print_str("}");
              return accumulator;
            }
          }
          if (printMode === PRINTMODE_LATEX && isplusone(exponent)) {
            accumulator += print_expr(base);
          } else {
            if (isadd(base) || isnegativenumber(base)) {
              accumulator += print_str("(");
              accumulator += print_expr(base);
              accumulator += print_str(")");
            } else if (car(base) === symbol(MULTIPLY) || car(base) === symbol(POWER)) {
              if (printMode !== PRINTMODE_LATEX) {
                accumulator += print_str("(");
              }
              accumulator += print_factor(base, true);
              if (printMode !== PRINTMODE_LATEX) {
                accumulator += print_str(")");
              }
            } else if (isNumericAtom(base) && (lessp(base, zero) || isfraction(base))) {
              accumulator += print_str("(");
              accumulator += print_factor(base);
              accumulator += print_str(")");
            } else {
              accumulator += print_factor(base);
            }
            if (printMode === PRINTMODE_HUMAN && !test_flag) {
              accumulator += print_str(power_str);
            } else {
              accumulator += print_str("^");
            }
            if (printMode === PRINTMODE_LATEX) {
              if (print_expr(exponent).length > 1) {
                accumulator += print_str("{");
                accumulator += print_expr(exponent);
                accumulator += print_str("}");
              } else {
                accumulator += print_expr(exponent);
              }
            } else if (iscons(exponent) || isfraction(exponent) || isNumericAtom(exponent) && lessp(exponent, zero)) {
              accumulator += print_str("(");
              accumulator += print_expr(exponent);
              accumulator += print_str(")");
            } else {
              accumulator += print_factor(exponent);
            }
          }
          return accumulator;
        };
        print_index_function = function(p11) {
          var accumulator;
          accumulator = "";
          p11 = cdr(p11);
          if (caar(p11) === symbol(ADD) || caar(p11) === symbol(MULTIPLY) || caar(p11) === symbol(POWER) || caar(p11) === symbol(FACTORIAL)) {
            accumulator += print_subexpr(car(p11));
          } else {
            accumulator += print_expr(car(p11));
          }
          accumulator += print_str("[");
          p11 = cdr(p11);
          if (iscons(p11)) {
            accumulator += print_expr(car(p11));
            p11 = cdr(p11);
            while (iscons(p11)) {
              accumulator += print_str(",");
              accumulator += print_expr(car(p11));
              p11 = cdr(p11);
            }
          }
          accumulator += print_str("]");
          return accumulator;
        };
        print_factor = function(p11, omitParens, pastFirstFactor) {
          var accumulator, base, exponent, fbody2, parameters, returned;
          accumulator = "";
          if (isNumericAtom(p11)) {
            if (pastFirstFactor && lessp(p11, zero)) {
              accumulator += "(";
            }
            accumulator += print_number(p11, pastFirstFactor);
            if (pastFirstFactor && lessp(p11, zero)) {
              accumulator += ")";
            }
            return accumulator;
          }
          if (isstr(p11)) {
            accumulator += print_str('"');
            accumulator += print_str(p11.str);
            accumulator += print_str('"');
            return accumulator;
          }
          if (istensor(p11)) {
            if (printMode === PRINTMODE_LATEX) {
              accumulator += print_tensor_latex(p11);
            } else {
              accumulator += print_tensor(p11);
            }
            return accumulator;
          }
          if (car(p11) === symbol(MULTIPLY)) {
            if (!omitParens) {
              if (sign_of_term(p11) === "-" || printMode !== PRINTMODE_LATEX) {
                if (printMode === PRINTMODE_LATEX) {
                  accumulator += print_str(" \\left (");
                } else {
                  accumulator += print_str("(");
                }
              }
            }
            accumulator += print_expr(p11);
            if (!omitParens) {
              if (sign_of_term(p11) === "-" || printMode !== PRINTMODE_LATEX) {
                if (printMode === PRINTMODE_LATEX) {
                  accumulator += print_str(" \\right ) ");
                } else {
                  accumulator += print_str(")");
                }
              }
            }
            return accumulator;
          } else if (isadd(p11)) {
            if (!omitParens) {
              accumulator += print_str("(");
            }
            accumulator += print_expr(p11);
            if (!omitParens) {
              accumulator += print_str(")");
            }
            return accumulator;
          }
          if (car(p11) === symbol(POWER)) {
            base = cadr(p11);
            exponent = caddr(p11);
            accumulator += print_power(base, exponent);
            return accumulator;
          }
          if (car(p11) === symbol(FUNCTION)) {
            fbody2 = cadr(p11);
            if (!codeGen) {
              parameters = caddr(p11);
              accumulator += print_str("function ");
              if (DEBUG) {
                console.log("emittedString from print_factor " + stringsEmittedByUserPrintouts);
              }
              returned = print_list(parameters);
              accumulator += returned;
              accumulator += print_str(" -> ");
            }
            accumulator += print_expr(fbody2);
            return accumulator;
          }
          if (car(p11) === symbol(PATTERN)) {
            accumulator += print_expr(caadr(p11));
            if (printMode === PRINTMODE_LATEX) {
              accumulator += print_str(" \\rightarrow ");
            } else {
              if (printMode === PRINTMODE_HUMAN && !test_flag) {
                accumulator += print_str(" -> ");
              } else {
                accumulator += print_str("->");
              }
            }
            accumulator += print_expr(car(cdr(cadr(p11))));
            return accumulator;
          }
          if (car(p11) === symbol(INDEX) && issymbol(cadr(p11))) {
            accumulator += print_index_function(p11);
            return accumulator;
          }
          if (car(p11) === symbol(FACTORIAL)) {
            accumulator += print_factorial_function(p11);
            return accumulator;
          } else if (car(p11) === symbol(ABS) && printMode === PRINTMODE_LATEX) {
            accumulator += print_ABS_latex(p11);
            return accumulator;
          } else if (car(p11) === symbol(SQRT) && printMode === PRINTMODE_LATEX) {
            accumulator += print_SQRT_latex(p11);
            return accumulator;
          } else if (car(p11) === symbol(TRANSPOSE)) {
            if (printMode === PRINTMODE_LATEX) {
              accumulator += print_TRANSPOSE_latex(p11);
              return accumulator;
            } else if (codeGen) {
              accumulator += print_TRANSPOSE_codegen(p11);
              return accumulator;
            }
          } else if (car(p11) === symbol(UNIT)) {
            if (codeGen) {
              accumulator += print_UNIT_codegen(p11);
              return accumulator;
            }
          } else if (car(p11) === symbol(INV)) {
            if (printMode === PRINTMODE_LATEX) {
              accumulator += print_INV_latex(p11);
              return accumulator;
            } else if (codeGen) {
              accumulator += print_INV_codegen(p11);
              return accumulator;
            }
          } else if (car(p11) === symbol(BINOMIAL) && printMode === PRINTMODE_LATEX) {
            accumulator += print_BINOMIAL_latex(p11);
            return accumulator;
          } else if (car(p11) === symbol(DEFINT) && printMode === PRINTMODE_LATEX) {
            accumulator += print_DEFINT_latex(p11);
            return accumulator;
          } else if (isinnerordot(p11)) {
            if (printMode === PRINTMODE_LATEX) {
              accumulator += print_DOT_latex(p11);
              return accumulator;
            } else if (codeGen) {
              accumulator += print_DOT_codegen(p11);
              return accumulator;
            }
          } else if (car(p11) === symbol(SIN)) {
            if (codeGen) {
              accumulator += print_SIN_codegen(p11);
              return accumulator;
            }
          } else if (car(p11) === symbol(COS)) {
            if (codeGen) {
              accumulator += print_COS_codegen(p11);
              return accumulator;
            }
          } else if (car(p11) === symbol(TAN)) {
            if (codeGen) {
              accumulator += print_TAN_codegen(p11);
              return accumulator;
            }
          } else if (car(p11) === symbol(ARCSIN)) {
            if (codeGen) {
              accumulator += print_ARCSIN_codegen(p11);
              return accumulator;
            }
          } else if (car(p11) === symbol(ARCCOS)) {
            if (codeGen) {
              accumulator += print_ARCCOS_codegen(p11);
              return accumulator;
            }
          } else if (car(p11) === symbol(ARCTAN)) {
            if (codeGen) {
              accumulator += print_ARCTAN_codegen(p11);
              return accumulator;
            }
          } else if (car(p11) === symbol(SUM)) {
            if (printMode === PRINTMODE_LATEX) {
              accumulator += print_SUM_latex(p11);
              return accumulator;
            } else if (codeGen) {
              accumulator += print_SUM_codegen(p11);
              return accumulator;
            }
          } else if (car(p11) === symbol(PRODUCT)) {
            if (printMode === PRINTMODE_LATEX) {
              accumulator += print_PRODUCT_latex(p11);
              return accumulator;
            } else if (codeGen) {
              accumulator += print_PRODUCT_codegen(p11);
              return accumulator;
            }
          } else if (car(p11) === symbol(FOR)) {
            if (codeGen) {
              accumulator += print_FOR_codegen(p11);
              return accumulator;
            }
          } else if (car(p11) === symbol(DO)) {
            if (codeGen) {
              accumulator += print_DO_codegen(p11);
              return accumulator;
            }
          } else if (car(p11) === symbol(TEST)) {
            if (codeGen) {
              accumulator += print_TEST_codegen(p11);
              return accumulator;
            }
            if (printMode === PRINTMODE_LATEX) {
              accumulator += print_TEST_latex(p11);
              return accumulator;
            }
          } else if (car(p11) === symbol(TESTLT)) {
            if (codeGen) {
              accumulator += "((" + print_expr(cadr(p11)) + ") < (" + print_expr(caddr(p11)) + "))";
              return accumulator;
            }
            if (printMode === PRINTMODE_LATEX) {
              accumulator += print_TESTLT_latex(p11);
              return accumulator;
            }
          } else if (car(p11) === symbol(TESTLE)) {
            if (codeGen) {
              accumulator += "((" + print_expr(cadr(p11)) + ") <= (" + print_expr(caddr(p11)) + "))";
              return accumulator;
            }
            if (printMode === PRINTMODE_LATEX) {
              accumulator += print_TESTLE_latex(p11);
              return accumulator;
            }
          } else if (car(p11) === symbol(TESTGT)) {
            if (codeGen) {
              accumulator += "((" + print_expr(cadr(p11)) + ") > (" + print_expr(caddr(p11)) + "))";
              return accumulator;
            }
            if (printMode === PRINTMODE_LATEX) {
              accumulator += print_TESTGT_latex(p11);
              return accumulator;
            }
          } else if (car(p11) === symbol(TESTGE)) {
            if (codeGen) {
              accumulator += "((" + print_expr(cadr(p11)) + ") >= (" + print_expr(caddr(p11)) + "))";
              return accumulator;
            }
            if (printMode === PRINTMODE_LATEX) {
              accumulator += print_TESTGE_latex(p11);
              return accumulator;
            }
          } else if (car(p11) === symbol(TESTEQ)) {
            if (codeGen) {
              accumulator += "((" + print_expr(cadr(p11)) + ") === (" + print_expr(caddr(p11)) + "))";
              return accumulator;
            }
            if (printMode === PRINTMODE_LATEX) {
              accumulator += print_TESTEQ_latex(p11);
              return accumulator;
            }
          } else if (car(p11) === symbol(FLOOR)) {
            if (codeGen) {
              accumulator += "Math.floor(" + print_expr(cadr(p11)) + ")";
              return accumulator;
            }
            if (printMode === PRINTMODE_LATEX) {
              accumulator += " \\lfloor {" + print_expr(cadr(p11)) + "} \\rfloor ";
              return accumulator;
            }
          } else if (car(p11) === symbol(CEILING)) {
            if (codeGen) {
              accumulator += "Math.ceiling(" + print_expr(cadr(p11)) + ")";
              return accumulator;
            }
            if (printMode === PRINTMODE_LATEX) {
              accumulator += " \\lceil {" + print_expr(cadr(p11)) + "} \\rceil ";
              return accumulator;
            }
          } else if (car(p11) === symbol(ROUND)) {
            if (codeGen) {
              accumulator += "Math.round(" + print_expr(cadr(p11)) + ")";
              return accumulator;
            }
          } else if (car(p11) === symbol(SETQ)) {
            if (codeGen) {
              accumulator += print_SETQ_codegen(p11);
              return accumulator;
            } else {
              accumulator += print_expr(cadr(p11));
              accumulator += print_str("=");
              accumulator += print_expr(caddr(p11));
              return accumulator;
            }
          }
          if (iscons(p11)) {
            accumulator += print_factor(car(p11));
            p11 = cdr(p11);
            if (!omitParens) {
              accumulator += print_str("(");
            }
            if (iscons(p11)) {
              accumulator += print_expr(car(p11));
              p11 = cdr(p11);
              while (iscons(p11)) {
                accumulator += print_str(",");
                accumulator += print_expr(car(p11));
                p11 = cdr(p11);
              }
            }
            if (!omitParens) {
              accumulator += print_str(")");
            }
            return accumulator;
          }
          if (p11 === symbol(DERIVATIVE)) {
            accumulator += print_char("d");
          } else if (p11 === symbol(E)) {
            if (codeGen) {
              accumulator += print_str("Math.E");
            } else {
              accumulator += print_str("e");
            }
          } else if (p11 === symbol(PI)) {
            if (printMode === PRINTMODE_LATEX) {
              accumulator += print_str("\\pi");
            } else {
              accumulator += print_str("pi");
            }
          } else {
            accumulator += print_str(get_printname(p11));
          }
          return accumulator;
        };
        print_list = function(p11) {
          var accumulator;
          accumulator = "";
          switch (p11.k) {
            case CONS:
              accumulator += "(";
              accumulator += print_list(car(p11));
              if (p11 === cdr(p11) && p11 !== symbol(NIL)) {
                console.log("oh no recursive!");
                debugger;
              }
              p11 = cdr(p11);
              while (iscons(p11)) {
                accumulator += " ";
                accumulator += print_list(car(p11));
                p11 = cdr(p11);
                if (p11 === cdr(p11) && p11 !== symbol(NIL)) {
                  console.log("oh no recursive!");
                  debugger;
                }
              }
              if (p11 !== symbol(NIL)) {
                accumulator += " . ";
                accumulator += print_list(p11);
              }
              accumulator += ")";
              break;
            case STR:
              accumulator += p11.str;
              break;
            //print_str("\"")
            case NUM:
            case DOUBLE:
              accumulator += print_number(p11, true);
              break;
            case SYM:
              accumulator += get_printname(p11);
              break;
            default:
              accumulator += "<tensor>";
          }
          return accumulator;
        };
        print_multiply_sign = function() {
          var accumulator;
          accumulator = "";
          if (printMode === PRINTMODE_LATEX) {
            if (printMode === PRINTMODE_HUMAN && !test_flag) {
              accumulator += print_str(" ");
            } else {
              return accumulator;
            }
          }
          if (printMode === PRINTMODE_HUMAN && !test_flag && !codeGen) {
            accumulator += print_str(" ");
          } else {
            accumulator += print_str("*");
          }
          return accumulator;
        };
        is_denominator = function(p11) {
          if (car(p11) === symbol(POWER) && cadr(p11) !== symbol(E) && isnegativeterm(caddr(p11))) {
            return 1;
          } else {
            return 0;
          }
        };
        any_denominators = function(p11) {
          var q;
          p11 = cdr(p11);
          while (iscons(p11)) {
            q = car(p11);
            if (is_denominator(q)) {
              return 1;
            }
            p11 = cdr(p11);
          }
          return 0;
        };
        YMAX = 1e4;
        glyph = function() {
          class glyph2 {
          }
          ;
          glyph2.prototype.c = 0;
          glyph2.prototype.x = 0;
          glyph2.prototype.y = 0;
          return glyph2;
        }.call(this);
        chartab = [];
        for (charTabIndex = i1 = 0, ref1 = YMAX; 0 <= ref1 ? i1 < ref1 : i1 > ref1; charTabIndex = 0 <= ref1 ? ++i1 : --i1) {
          chartab[charTabIndex] = new glyph();
        }
        yindex = 0;
        level = 0;
        emit_x = 0;
        expr_level = 0;
        display_flag = 0;
        printchar_nowrap = function(character) {
          var accumulator;
          accumulator = "";
          accumulator += character;
          return accumulator;
        };
        printchar = function(character) {
          return printchar_nowrap(character);
        };
        print2dascii = function(p11) {
          var beenPrinted, h5, w2, y2;
          h5 = 0;
          w2 = 0;
          y2 = 0;
          save();
          yindex = 0;
          level = 0;
          emit_x = 0;
          emit_top_expr(p11);
          [h5, w2, y2] = get_size(0, yindex);
          if (w2 > 100) {
            printline(p11);
            restore();
            return;
          }
          beenPrinted = print_glyphs();
          restore();
          return beenPrinted;
        };
        emit_top_expr = function(p11) {
          if (car(p11) === symbol(SETQ)) {
            emit_expr(cadr(p11));
            __emit_str(" = ");
            emit_expr(caddr(p11));
            return;
          }
          if (istensor(p11)) {
            return emit_tensor(p11);
          } else {
            return emit_expr(p11);
          }
        };
        will_be_displayed_as_fraction = function(p11) {
          if (level > 0) {
            return 0;
          }
          if (isfraction(p11)) {
            return 1;
          }
          if (car(p11) !== symbol(MULTIPLY)) {
            return 0;
          }
          if (isfraction(cadr(p11))) {
            return 1;
          }
          while (iscons(p11)) {
            if (isdenominator(car(p11))) {
              return 1;
            }
            p11 = cdr(p11);
          }
          return 0;
        };
        emit_expr = function(p11) {
          expr_level++;
          if (car(p11) === symbol(ADD)) {
            p11 = cdr(p11);
            if (__is_negative(car(p11))) {
              __emit_char("-");
              if (will_be_displayed_as_fraction(car(p11))) {
                __emit_char(" ");
              }
            }
            emit_term(car(p11));
            p11 = cdr(p11);
            while (iscons(p11)) {
              if (__is_negative(car(p11))) {
                __emit_char(" ");
                __emit_char("-");
                __emit_char(" ");
              } else {
                __emit_char(" ");
                __emit_char("+");
                __emit_char(" ");
              }
              emit_term(car(p11));
              p11 = cdr(p11);
            }
          } else {
            if (__is_negative(p11)) {
              __emit_char("-");
              if (will_be_displayed_as_fraction(p11)) {
                __emit_char(" ");
              }
            }
            emit_term(p11);
          }
          return expr_level--;
        };
        emit_unsigned_expr = function(p11) {
          var results;
          if (car(p11) === symbol(ADD)) {
            p11 = cdr(p11);
            emit_term(car(p11));
            p11 = cdr(p11);
            results = [];
            while (iscons(p11)) {
              if (__is_negative(car(p11))) {
                __emit_char(" ");
                __emit_char("-");
                __emit_char(" ");
              } else {
                __emit_char(" ");
                __emit_char("+");
                __emit_char(" ");
              }
              emit_term(car(p11));
              results.push(p11 = cdr(p11));
            }
            return results;
          } else {
            return emit_term(p11);
          }
        };
        __is_negative = function(p11) {
          if (isnegativenumber(p11)) {
            return 1;
          }
          if (car(p11) === symbol(MULTIPLY) && isnegativenumber(cadr(p11))) {
            return 1;
          }
          return 0;
        };
        emit_term = function(p11) {
          var n9;
          if (car(p11) === symbol(MULTIPLY)) {
            n9 = count_denominators(p11);
            if (n9 && level === 0) {
              return emit_fraction(p11, n9);
            } else {
              return emit_multiply(p11, n9);
            }
          } else {
            return emit_factor(p11);
          }
        };
        isdenominator = function(p11) {
          if (car(p11) === symbol(POWER) && cadr(p11) !== symbol(E) && __is_negative(caddr(p11))) {
            return 1;
          } else {
            return 0;
          }
        };
        count_denominators = function(p11) {
          var count2, q;
          count2 = 0;
          p11 = cdr(p11);
          while (iscons(p11)) {
            q = car(p11);
            if (isdenominator(q)) {
              count2++;
            }
            p11 = cdr(p11);
          }
          return count2;
        };
        emit_multiply = function(p11, n9) {
          var results;
          if (n9 === 0) {
            p11 = cdr(p11);
            if (isplusone(car(p11)) || isminusone(car(p11))) {
              p11 = cdr(p11);
            }
            emit_factor(car(p11));
            p11 = cdr(p11);
            results = [];
            while (iscons(p11)) {
              __emit_char(" ");
              emit_factor(car(p11));
              results.push(p11 = cdr(p11));
            }
            return results;
          } else {
            emit_numerators(p11);
            __emit_char("/");
            if (n9 > 1 || isfraction(cadr(p11))) {
              __emit_char("(");
              emit_denominators(p11);
              return __emit_char(")");
            } else {
              return emit_denominators(p11);
            }
          }
        };
        emit_fraction = function(p11, d3) {
          var count2, doNothing, k1, k2, n9, x2;
          count2 = 0;
          k1 = 0;
          k2 = 0;
          n9 = 0;
          x2 = 0;
          save();
          p3 = one;
          p4 = one;
          if (isrational(cadr(p11))) {
            push(cadr(p11));
            mp_numerator();
            absval();
            p3 = pop();
            push(cadr(p11));
            mp_denominator();
            p4 = pop();
          }
          if (isdouble(cadr(p11))) {
            push(cadr(p11));
            absval();
            p3 = pop();
          }
          if (isplusone(p3)) {
            n9 = 0;
          } else {
            n9 = 1;
          }
          p1 = cdr(p11);
          if (isNumericAtom(car(p1))) {
            p1 = cdr(p1);
          }
          while (iscons(p1)) {
            p2 = car(p1);
            if (isdenominator(p2)) {
              doNothing = 1;
            } else {
              n9++;
            }
            p1 = cdr(p1);
          }
          x2 = emit_x;
          k1 = yindex;
          count2 = 0;
          if (!isplusone(p3)) {
            emit_number(p3, 0);
            count2++;
          }
          p1 = cdr(p11);
          if (isNumericAtom(car(p1))) {
            p1 = cdr(p1);
          }
          while (iscons(p1)) {
            p2 = car(p1);
            if (isdenominator(p2)) {
              doNothing = 1;
            } else {
              if (count2 > 0) {
                __emit_char(" ");
              }
              if (n9 === 1) {
                emit_expr(p2);
              } else {
                emit_factor(p2);
              }
              count2++;
            }
            p1 = cdr(p1);
          }
          if (count2 === 0) {
            __emit_char("1");
          }
          k2 = yindex;
          count2 = 0;
          if (!isplusone(p4)) {
            emit_number(p4, 0);
            count2++;
            d3++;
          }
          p1 = cdr(p11);
          if (isrational(car(p1))) {
            p1 = cdr(p1);
          }
          while (iscons(p1)) {
            p2 = car(p1);
            if (isdenominator(p2)) {
              if (count2 > 0) {
                __emit_char(" ");
              }
              emit_denominator(p2, d3);
              count2++;
            }
            p1 = cdr(p1);
          }
          fixup_fraction(x2, k1, k2);
          return restore();
        };
        emit_numerators = function(p11) {
          var doNothing, n9;
          save();
          n9 = 0;
          p1 = one;
          p11 = cdr(p11);
          if (isrational(car(p11))) {
            push(car(p11));
            mp_numerator();
            absval();
            p1 = pop();
            p11 = cdr(p11);
          } else if (isdouble(car(p11))) {
            push(car(p11));
            absval();
            p1 = pop();
            p11 = cdr(p11);
          }
          n9 = 0;
          if (!isplusone(p1)) {
            emit_number(p1, 0);
            n9++;
          }
          while (iscons(p11)) {
            if (isdenominator(car(p11))) {
              doNothing = 1;
            } else {
              if (n9 > 0) {
                __emit_char(" ");
              }
              emit_factor(car(p11));
              n9++;
            }
            p11 = cdr(p11);
          }
          if (n9 === 0) {
            __emit_char("1");
          }
          return restore();
        };
        emit_denominators = function(p11) {
          var n9;
          save();
          n9 = 0;
          p11 = cdr(p11);
          if (isfraction(car(p11))) {
            push(car(p11));
            mp_denominator();
            p1 = pop();
            emit_number(p1, 0);
            n9++;
            p11 = cdr(p11);
          }
          while (iscons(p11)) {
            if (isdenominator(car(p11))) {
              if (n9 > 0) {
                __emit_char(" ");
              }
              emit_denominator(car(p11), 0);
              n9++;
            }
            p11 = cdr(p11);
          }
          return restore();
        };
        emit_factor = function(p11) {
          if (istensor(p11)) {
            if (level === 0) {
              emit_flat_tensor(p11);
            } else {
              emit_flat_tensor(p11);
            }
            return;
          }
          if (isdouble(p11)) {
            emit_number(p11, 0);
            return;
          }
          if (car(p11) === symbol(ADD) || car(p11) === symbol(MULTIPLY)) {
            emit_subexpr(p11);
            return;
          }
          if (car(p11) === symbol(POWER)) {
            emit_power(p11);
            return;
          }
          if (iscons(p11)) {
            emit_function(p11);
            return;
          }
          if (isNumericAtom(p11)) {
            if (level === 0) {
              emit_numerical_fraction(p11);
            } else {
              emit_number(p11, 0);
            }
            return;
          }
          if (issymbol(p11)) {
            emit_symbol(p11);
            return;
          }
          if (isstr(p11)) {
            emit_string(p11);
          }
        };
        emit_numerical_fraction = function(p11) {
          var k1, k2, x2;
          k1 = 0;
          k2 = 0;
          x2 = 0;
          save();
          push(p11);
          mp_numerator();
          absval();
          p3 = pop();
          push(p11);
          mp_denominator();
          p4 = pop();
          if (isplusone(p4)) {
            emit_number(p3, 0);
            restore();
            return;
          }
          x2 = emit_x;
          k1 = yindex;
          emit_number(p3, 0);
          k2 = yindex;
          emit_number(p4, 0);
          fixup_fraction(x2, k1, k2);
          return restore();
        };
        isfactor = function(p11) {
          if (iscons(p11) && car(p11) !== symbol(ADD) && car(p11) !== symbol(MULTIPLY) && car(p11) !== symbol(POWER)) {
            return 1;
          }
          if (issymbol(p11)) {
            return 1;
          }
          if (isfraction(p11)) {
            return 0;
          }
          if (isnegativenumber(p11)) {
            return 0;
          }
          if (isNumericAtom(p11)) {
            return 1;
          }
          return 0;
        };
        emit_power = function(p11) {
          var k1, k2, x2;
          k1 = 0;
          k2 = 0;
          x2 = 0;
          if (cadr(p11) === symbol(E)) {
            __emit_str("exp(");
            emit_expr(caddr(p11));
            __emit_char(")");
            return;
          }
          if (level > 0) {
            if (isminusone(caddr(p11))) {
              __emit_char("1");
              __emit_char("/");
              if (isfactor(cadr(p11))) {
                emit_factor(cadr(p11));
              } else {
                emit_subexpr(cadr(p11));
              }
            } else {
              if (isfactor(cadr(p11))) {
                emit_factor(cadr(p11));
              } else {
                emit_subexpr(cadr(p11));
              }
              __emit_char("^");
              if (isfactor(caddr(p11))) {
                emit_factor(caddr(p11));
              } else {
                emit_subexpr(caddr(p11));
              }
            }
            return;
          }
          if (__is_negative(caddr(p11))) {
            x2 = emit_x;
            k1 = yindex;
            __emit_char("1");
            k2 = yindex;
            emit_denominator(p11, 1);
            fixup_fraction(x2, k1, k2);
            return;
          }
          k1 = yindex;
          if (isfactor(cadr(p11))) {
            emit_factor(cadr(p11));
          } else {
            emit_subexpr(cadr(p11));
          }
          k2 = yindex;
          level++;
          emit_expr(caddr(p11));
          level--;
          return fixup_power(k1, k2);
        };
        emit_denominator = function(p11, n9) {
          var k1, k2;
          k1 = 0;
          k2 = 0;
          if (isminusone(caddr(p11))) {
            if (n9 === 1) {
              emit_expr(cadr(p11));
            } else {
              emit_factor(cadr(p11));
            }
            return;
          }
          k1 = yindex;
          if (isfactor(cadr(p11))) {
            emit_factor(cadr(p11));
          } else {
            emit_subexpr(cadr(p11));
          }
          k2 = yindex;
          level++;
          emit_unsigned_expr(caddr(p11));
          level--;
          return fixup_power(k1, k2);
        };
        emit_function = function(p11) {
          if (car(p11) === symbol(INDEX) && issymbol(cadr(p11))) {
            emit_index_function(p11);
            return;
          }
          if (car(p11) === symbol(FACTORIAL)) {
            emit_factorial_function(p11);
            return;
          }
          if (car(p11) === symbol(DERIVATIVE)) {
            __emit_char("d");
          } else {
            emit_symbol(car(p11));
          }
          __emit_char("(");
          p11 = cdr(p11);
          if (iscons(p11)) {
            emit_expr(car(p11));
            p11 = cdr(p11);
            while (iscons(p11)) {
              __emit_char(",");
              emit_expr(car(p11));
              p11 = cdr(p11);
            }
          }
          return __emit_char(")");
        };
        emit_index_function = function(p11) {
          p11 = cdr(p11);
          if (caar(p11) === symbol(ADD) || caar(p11) === symbol(MULTIPLY) || caar(p11) === symbol(POWER) || caar(p11) === symbol(FACTORIAL)) {
            emit_subexpr(car(p11));
          } else {
            emit_expr(car(p11));
          }
          __emit_char("[");
          p11 = cdr(p11);
          if (iscons(p11)) {
            emit_expr(car(p11));
            p11 = cdr(p11);
            while (iscons(p11)) {
              __emit_char(",");
              emit_expr(car(p11));
              p11 = cdr(p11);
            }
          }
          return __emit_char("]");
        };
        emit_factorial_function = function(p11) {
          p11 = cadr(p11);
          if (isfraction(p11) || car(p11) === symbol(ADD) || car(p11) === symbol(MULTIPLY) || car(p11) === symbol(POWER) || car(p11) === symbol(FACTORIAL)) {
            emit_subexpr(p11);
          } else {
            emit_expr(p11);
          }
          return __emit_char("!");
        };
        emit_subexpr = function(p11) {
          __emit_char("(");
          emit_expr(p11);
          return __emit_char(")");
        };
        emit_symbol = function(p11) {
          var i5, j12, pPrintName, ref2, results;
          i5 = 0;
          if (p11 === symbol(E)) {
            __emit_str("exp(1)");
            return;
          }
          pPrintName = get_printname(p11);
          results = [];
          for (i5 = j12 = 0, ref2 = pPrintName.length; 0 <= ref2 ? j12 < ref2 : j12 > ref2; i5 = 0 <= ref2 ? ++j12 : --j12) {
            results.push(__emit_char(pPrintName[i5]));
          }
          return results;
        };
        emit_string = function(p11) {
          var i5, j12, pString, ref2;
          i5 = 0;
          pString = p11.str;
          __emit_char('"');
          for (i5 = j12 = 0, ref2 = pString.length; 0 <= ref2 ? j12 < ref2 : j12 > ref2; i5 = 0 <= ref2 ? ++j12 : --j12) {
            __emit_char(pString[i5]);
          }
          return __emit_char('"');
        };
        fixup_fraction = function(x2, k1, k2) {
          var dx, dy, h1, h22, i5, j12, ref2, results, w2, w1, w22, y2, y1, y22;
          dx = 0;
          dy = 0;
          i5 = 0;
          w2 = 0;
          y2 = 0;
          h1 = 0;
          w1 = 0;
          y1 = 0;
          h22 = 0;
          w22 = 0;
          y22 = 0;
          [h1, w1, y1] = get_size(k1, k2);
          [h22, w22, y22] = get_size(k2, yindex);
          if (w22 > w1) {
            dx = (w22 - w1) / 2;
          } else {
            dx = 0;
          }
          dx++;
          y2 = y1 + h1 - 1;
          dy = -y2 - 1;
          move(k1, k2, dx, dy);
          if (w22 > w1) {
            dx = -w1;
          } else {
            dx = -w1 + (w1 - w22) / 2;
          }
          dx++;
          dy = -y22 + 1;
          move(k2, yindex, dx, dy);
          if (w22 > w1) {
            w2 = w22;
          } else {
            w2 = w1;
          }
          w2 += 2;
          emit_x = x2;
          results = [];
          for (i5 = j12 = 0, ref2 = w2; 0 <= ref2 ? j12 < ref2 : j12 > ref2; i5 = 0 <= ref2 ? ++j12 : --j12) {
            results.push(__emit_char("-"));
          }
          return results;
        };
        fixup_power = function(k1, k2) {
          var dy, h1, h22, w1, w2, y1, y2;
          dy = 0;
          h1 = 0;
          w1 = 0;
          y1 = 0;
          h22 = 0;
          w2 = 0;
          y2 = 0;
          [h1, w1, y1] = get_size(k1, k2);
          [h22, w2, y2] = get_size(k2, yindex);
          dy = -y2 - h22 + 1;
          dy += y1 - 1;
          return move(k2, yindex, 0, dy);
        };
        move = function(j2, k2, dx, dy) {
          var i5, j12, ref2, ref3, results;
          i5 = 0;
          results = [];
          for (i5 = j12 = ref2 = j2, ref3 = k2; ref2 <= ref3 ? j12 < ref3 : j12 > ref3; i5 = ref2 <= ref3 ? ++j12 : --j12) {
            chartab[i5].x += dx;
            results.push(chartab[i5].y += dy);
          }
          return results;
        };
        get_size = function(j2, k2) {
          var h5, i5, j12, max_x, max_y, min_x, min_y, ref2, ref3, w2, y2;
          i5 = 0;
          min_x = chartab[j2].x;
          max_x = chartab[j2].x;
          min_y = chartab[j2].y;
          max_y = chartab[j2].y;
          for (i5 = j12 = ref2 = j2 + 1, ref3 = k2; ref2 <= ref3 ? j12 < ref3 : j12 > ref3; i5 = ref2 <= ref3 ? ++j12 : --j12) {
            if (chartab[i5].x < min_x) {
              min_x = chartab[i5].x;
            }
            if (chartab[i5].x > max_x) {
              max_x = chartab[i5].x;
            }
            if (chartab[i5].y < min_y) {
              min_y = chartab[i5].y;
            }
            if (chartab[i5].y > max_y) {
              max_y = chartab[i5].y;
            }
          }
          h5 = max_y - min_y + 1;
          w2 = max_x - min_x + 1;
          y2 = min_y;
          return [h5, w2, y2];
        };
        displaychar = function(c6) {
          return __emit_char(c6);
        };
        __emit_char = function(c6) {
          if (yindex === YMAX) {
            return;
          }
          if (chartab[yindex] == null) {
            debugger;
          }
          chartab[yindex].c = c6;
          chartab[yindex].x = emit_x;
          chartab[yindex].y = 0;
          yindex++;
          return emit_x++;
        };
        __emit_str = function(s7) {
          var i5, j12, ref2, results;
          i5 = 0;
          results = [];
          for (i5 = j12 = 0, ref2 = s7.length; 0 <= ref2 ? j12 < ref2 : j12 > ref2; i5 = 0 <= ref2 ? ++j12 : --j12) {
            results.push(__emit_char(s7[i5]));
          }
          return results;
        };
        emit_number = function(p11, emit_sign) {
          var i5, j12, l1, m1, ref2, ref3, ref4, results, results1, tmpString;
          tmpString = "";
          i5 = 0;
          switch (p11.k) {
            case NUM:
              tmpString = p11.q.a.toString();
              if (tmpString[0] === "-" && emit_sign === 0) {
                tmpString = tmpString.substring(1);
              }
              for (i5 = j12 = 0, ref2 = tmpString.length; 0 <= ref2 ? j12 < ref2 : j12 > ref2; i5 = 0 <= ref2 ? ++j12 : --j12) {
                __emit_char(tmpString[i5]);
              }
              tmpString = p11.q.b.toString();
              if (tmpString === "1") {
                break;
              }
              __emit_char("/");
              results = [];
              for (i5 = l1 = 0, ref3 = tmpString.length; 0 <= ref3 ? l1 < ref3 : l1 > ref3; i5 = 0 <= ref3 ? ++l1 : --l1) {
                results.push(__emit_char(tmpString[i5]));
              }
              return results;
              break;
            case DOUBLE:
              tmpString = doubleToReasonableString(p11.d);
              if (tmpString[0] === "-" && emit_sign === 0) {
                tmpString = tmpString.substring(1);
              }
              results1 = [];
              for (i5 = m1 = 0, ref4 = tmpString.length; 0 <= ref4 ? m1 < ref4 : m1 > ref4; i5 = 0 <= ref4 ? ++m1 : --m1) {
                results1.push(__emit_char(tmpString[i5]));
              }
              return results1;
          }
        };
        cmpGlyphs = function(a4, b2) {
          if (a4.y < b2.y) {
            return -1;
          }
          if (a4.y > b2.y) {
            return 1;
          }
          if (a4.x < b2.x) {
            return -1;
          }
          if (a4.x > b2.x) {
            return 1;
          }
          return 0;
        };
        print_glyphs = function() {
          var accumulator, i5, j12, ref2, subsetOfStack, x2, y2;
          i5 = 0;
          accumulator = "";
          subsetOfStack = chartab.slice(0, yindex);
          subsetOfStack.sort(cmpGlyphs);
          chartab = [].concat(subsetOfStack).concat(chartab.slice(yindex));
          x2 = 0;
          y2 = chartab[0].y;
          for (i5 = j12 = 0, ref2 = yindex; 0 <= ref2 ? j12 < ref2 : j12 > ref2; i5 = 0 <= ref2 ? ++j12 : --j12) {
            while (chartab[i5].y > y2) {
              accumulator += printchar("\n");
              x2 = 0;
              y2++;
            }
            while (chartab[i5].x > x2) {
              accumulator += printchar_nowrap(" ");
              x2++;
            }
            accumulator += printchar_nowrap(chartab[i5].c);
            x2++;
          }
          return accumulator;
        };
        buffer = "";
        getdisplaystr = function() {
          yindex = 0;
          level = 0;
          emit_x = 0;
          emit_expr(pop());
          fill_buf();
          return buffer;
        };
        fill_buf = function() {
          var i5, j12, ref2, sIndex, subsetOfStack, tmpBuffer, x2, y2;
          tmpBuffer = buffer;
          sIndex = 0;
          i5 = 0;
          subsetOfStack = chartab.slice(0, yindex);
          subsetOfStack.sort(cmpGlyphs);
          chartab = [].concat(subsetOfStack).concat(chartab.slice(yindex));
          x2 = 0;
          y2 = chartab[0].y;
          for (i5 = j12 = 0, ref2 = yindex; 0 <= ref2 ? j12 < ref2 : j12 > ref2; i5 = 0 <= ref2 ? ++j12 : --j12) {
            while (chartab[i5].y > y2) {
              tmpBuffer[sIndex++] = "\n";
              x2 = 0;
              y2++;
            }
            while (chartab[i5].x > x2) {
              tmpBuffer[sIndex++] = " ";
              x2++;
            }
            tmpBuffer[sIndex++] = chartab[i5].c;
            x2++;
          }
          return tmpBuffer[sIndex++] = "\n";
        };
        N = 100;
        oneElement = function() {
          class oneElement2 {
          }
          ;
          oneElement2.prototype.x = 0;
          oneElement2.prototype.y = 0;
          oneElement2.prototype.h = 0;
          oneElement2.prototype.w = 0;
          oneElement2.prototype.index = 0;
          oneElement2.prototype.count = 0;
          return oneElement2;
        }.call(this);
        elem = [];
        for (elelmIndex = j1 = 0; j1 < 1e4; elelmIndex = ++j1) {
          elem[elelmIndex] = new oneElement();
        }
        SPACE_BETWEEN_COLUMNS = 3;
        SPACE_BETWEEN_ROWS = 1;
        emit_tensor = function(p11) {
          var col, dx, dy, eh, ew, h5, i5, l1, m1, n9, n1, ncol, nrow, o1, ref2, ref3, ref4, ref5, row, w2, x2, y2;
          i5 = 0;
          n9 = 0;
          nrow = 0;
          ncol = 0;
          x2 = 0;
          y2 = 0;
          h5 = 0;
          w2 = 0;
          dx = 0;
          dy = 0;
          eh = 0;
          ew = 0;
          row = 0;
          col = 0;
          if (p11.tensor.ndim > 2) {
            emit_flat_tensor(p11);
            return;
          }
          nrow = p11.tensor.dim[0];
          if (p11.tensor.ndim === 2) {
            ncol = p11.tensor.dim[1];
          } else {
            ncol = 1;
          }
          n9 = nrow * ncol;
          if (n9 > N) {
            emit_flat_tensor(p11);
            return;
          }
          x2 = emit_x;
          for (i5 = l1 = 0, ref2 = n9; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
            elem[i5].index = yindex;
            elem[i5].x = emit_x;
            emit_expr(p11.tensor.elem[i5]);
            elem[i5].count = yindex - elem[i5].index;
            [elem[i5].h, elem[i5].w, elem[i5].y] = get_size(elem[i5].index, yindex);
          }
          eh = 0;
          ew = 0;
          for (i5 = m1 = 0, ref3 = n9; 0 <= ref3 ? m1 < ref3 : m1 > ref3; i5 = 0 <= ref3 ? ++m1 : --m1) {
            if (elem[i5].h > eh) {
              eh = elem[i5].h;
            }
            if (elem[i5].w > ew) {
              ew = elem[i5].w;
            }
          }
          h5 = nrow * eh + (nrow - 1) * SPACE_BETWEEN_ROWS;
          w2 = ncol * ew + (ncol - 1) * SPACE_BETWEEN_COLUMNS;
          y2 = -(h5 / 2);
          for (row = n1 = 0, ref4 = nrow; 0 <= ref4 ? n1 < ref4 : n1 > ref4; row = 0 <= ref4 ? ++n1 : --n1) {
            for (col = o1 = 0, ref5 = ncol; 0 <= ref5 ? o1 < ref5 : o1 > ref5; col = 0 <= ref5 ? ++o1 : --o1) {
              i5 = row * ncol + col;
              dx = x2 - elem[i5].x;
              dy = y2 - elem[i5].y;
              move(elem[i5].index, elem[i5].index + elem[i5].count, dx, dy);
              dx = 0;
              if (col > 0) {
                dx = col * (ew + SPACE_BETWEEN_COLUMNS);
              }
              dy = 0;
              if (row > 0) {
                dy = row * (eh + SPACE_BETWEEN_ROWS);
              }
              dx += (ew - elem[i5].w) / 2;
              dy += (eh - elem[i5].h) / 2;
              move(elem[i5].index, elem[i5].index + elem[i5].count, dx, dy);
            }
          }
          return emit_x = x2 + w2;
        };
        emit_flat_tensor = function(p11) {
          return emit_tensor_inner(p11, 0, 0);
        };
        emit_tensor_inner = function(p11, j2, k2) {
          var i5, l1, ref2;
          i5 = 0;
          __emit_char("(");
          for (i5 = l1 = 0, ref2 = p11.tensor.dim[j2]; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
            if (j2 + 1 === p11.tensor.ndim) {
              emit_expr(p11.tensor.elem[k2]);
              k2 = k2 + 1;
            } else {
              k2 = emit_tensor_inner(p11, j2 + 1, k2);
            }
            if (i5 + 1 < p11.tensor.dim[j2]) {
              __emit_char(",");
            }
          }
          __emit_char(")");
          return k2;
        };
        Eval_product = function() {
          var body, i5, indexVariable, j2, k2, l1, oldIndexVariableValue, ref2, ref3;
          i5 = 0;
          j2 = 0;
          k2 = 0;
          body = cadr(p1);
          indexVariable = caddr(p1);
          if (!issymbol(indexVariable)) {
            stop("sum: 2nd arg?");
          }
          push(cadddr(p1));
          Eval();
          j2 = pop_integer();
          if (isNaN(j2)) {
            push(p1);
            return;
          }
          push(caddddr(p1));
          Eval();
          k2 = pop_integer();
          if (isNaN(k2)) {
            push(p1);
            return;
          }
          oldIndexVariableValue = get_binding(indexVariable);
          push_integer(1);
          for (i5 = l1 = ref2 = j2, ref3 = k2; ref2 <= ref3 ? l1 <= ref3 : l1 >= ref3; i5 = ref2 <= ref3 ? ++l1 : --l1) {
            push_integer(i5);
            p5 = pop();
            set_binding(indexVariable, p5);
            push(body);
            Eval();
            if (DEBUG) {
              console.log("product - factor 1: " + stack[tos - 1].toString());
              console.log("product - factor 2: " + stack[tos - 2].toString());
            }
            multiply();
            if (DEBUG) {
              console.log("product - result: " + stack[tos - 1].toString());
            }
          }
          return set_binding(indexVariable, oldIndexVariableValue);
        };
        qadd = function() {
          var gcdBetweenNumeratorAndDenominator, qadd_ab, qadd_ba, qadd_denominator, qadd_frac1, qadd_frac2, qadd_numerator, resultSum;
          qadd_frac2 = pop();
          qadd_frac1 = pop();
          qadd_ab = mmul(qadd_frac1.q.a, qadd_frac2.q.b);
          qadd_ba = mmul(qadd_frac1.q.b, qadd_frac2.q.a);
          qadd_numerator = madd(qadd_ab, qadd_ba);
          if (MZERO(qadd_numerator)) {
            push(zero);
            return;
          }
          qadd_denominator = mmul(qadd_frac1.q.b, qadd_frac2.q.b);
          gcdBetweenNumeratorAndDenominator = mgcd(qadd_numerator, qadd_denominator);
          gcdBetweenNumeratorAndDenominator = makeSignSameAs(gcdBetweenNumeratorAndDenominator, qadd_denominator);
          resultSum = new U();
          resultSum.k = NUM;
          resultSum.q.a = mdiv(qadd_numerator, gcdBetweenNumeratorAndDenominator);
          resultSum.q.b = mdiv(qadd_denominator, gcdBetweenNumeratorAndDenominator);
          return push(resultSum);
        };
        qdiv = function() {
          var aa, bb, c6;
          save();
          p2 = pop();
          p1 = pop();
          if (MZERO(p2.q.a)) {
            stop("divide by zero");
          }
          if (MZERO(p1.q.a)) {
            push(zero);
            restore();
            return;
          }
          aa = mmul(p1.q.a, p2.q.b);
          bb = mmul(p1.q.b, p2.q.a);
          c6 = mgcd(aa, bb);
          c6 = makeSignSameAs(c6, bb);
          p1 = new U();
          p1.k = NUM;
          p1.q.a = mdiv(aa, c6);
          p1.q.b = mdiv(bb, c6);
          push(p1);
          return restore();
        };
        qmul = function() {
          var aa, bb, c6;
          save();
          p2 = pop();
          p1 = pop();
          if (MZERO(p1.q.a) || MZERO(p2.q.a)) {
            push(zero);
            restore();
            return;
          }
          aa = mmul(p1.q.a, p2.q.a);
          bb = mmul(p1.q.b, p2.q.b);
          c6 = mgcd(aa, bb);
          c6 = makeSignSameAs(c6, bb);
          p1 = new U();
          p1.k = NUM;
          p1.q.a = mdiv(aa, c6);
          p1.q.b = mdiv(bb, c6);
          push(p1);
          return restore();
        };
        qpow = function() {
          save();
          qpowf();
          return restore();
        };
        qpowf = function() {
          var a4, b2, expo, t5, x2, y2;
          expo = 0;
          p2 = pop();
          p1 = pop();
          if (isplusone(p1) || isZeroAtomOrTensor(p2)) {
            push_integer(1);
            return;
          }
          if (isminusone(p1) && isoneovertwo(p2)) {
            push(imaginaryunit);
            return;
          }
          if (isZeroAtomOrTensor(p1)) {
            if (isnegativenumber(p2)) {
              stop("divide by zero");
            }
            push(zero);
            return;
          }
          if (isplusone(p2)) {
            push(p1);
            return;
          }
          if (isinteger(p2)) {
            push(p2);
            expo = pop_integer();
            if (isNaN(expo)) {
              push_symbol(POWER);
              push(p1);
              push(p2);
              list(3);
              return;
            }
            x2 = mpow(p1.q.a, Math.abs(expo));
            y2 = mpow(p1.q.b, Math.abs(expo));
            if (expo < 0) {
              t5 = x2;
              x2 = y2;
              y2 = t5;
              x2 = makeSignSameAs(x2, y2);
              y2 = makePositive(y2);
            }
            p3 = new U();
            p3.k = NUM;
            p3.q.a = x2;
            p3.q.b = y2;
            push(p3);
            return;
          }
          if (isminusone(p1)) {
            push(p2);
            normalize_angle();
            return;
          }
          if (isnegativenumber(p1)) {
            push(p1);
            negate();
            push(p2);
            qpow();
            push_integer(-1);
            push(p2);
            qpow();
            multiply();
            return;
          }
          if (!isinteger(p1)) {
            push(p1);
            mp_numerator();
            push(p2);
            qpow();
            push(p1);
            mp_denominator();
            push(p2);
            negate();
            qpow();
            multiply();
            return;
          }
          if (is_small_integer(p1)) {
            push(p1);
            push(p2);
            quickfactor();
            return;
          }
          if (!isSmall(p2.q.a) || !isSmall(p2.q.b)) {
            push_symbol(POWER);
            push(p1);
            push(p2);
            list(3);
            return;
          }
          a4 = p2.q.a;
          b2 = p2.q.b;
          x2 = mroot(p1.q.a, b2);
          if (x2 === 0) {
            push_symbol(POWER);
            push(p1);
            push(p2);
            list(3);
            return;
          }
          y2 = mpow(x2, a4);
          p3 = new U();
          p3.k = NUM;
          if (p2.q.a.isNegative()) {
            p3.q.a = bigInt(1);
            p3.q.b = y2;
          } else {
            p3.q.a = y2;
            p3.q.b = bigInt(1);
          }
          return push(p3);
        };
        normalize_angle = function() {
          save();
          p1 = pop();
          if (isinteger(p1)) {
            if (p1.q.a.isOdd()) {
              push_integer(-1);
            } else {
              push_integer(1);
            }
            restore();
            return;
          }
          push(p1);
          bignum_truncate();
          p2 = pop();
          if (isnegativenumber(p1)) {
            push(p2);
            push_integer(-1);
            add();
            p2 = pop();
          }
          push(p1);
          push(p2);
          subtract();
          p3 = pop();
          push_symbol(POWER);
          push_integer(-1);
          push(p3);
          list(3);
          if (p2.q.a.isOdd()) {
            negate();
          }
          return restore();
        };
        is_small_integer = function(p11) {
          return isSmall(p11.q.a);
        };
        quickfactor = function() {
          var h5, i5, l1, n9, ref2, stackIndex;
          i5 = 0;
          save();
          p2 = pop();
          p1 = pop();
          h5 = tos;
          push(p1);
          factor_small_number();
          n9 = tos - h5;
          stackIndex = h5;
          for (i5 = l1 = 0, ref2 = n9; l1 < ref2; i5 = l1 += 2) {
            push(stack[stackIndex + i5]);
            push(stack[stackIndex + i5 + 1]);
            push(p2);
            multiply();
            quickpower();
          }
          multiply_all(tos - h5 - n9);
          p1 = pop();
          moveTos(h5);
          push(p1);
          return restore();
        };
        quickpower = function() {
          var expo;
          expo = 0;
          save();
          p2 = pop();
          p1 = pop();
          push(p2);
          bignum_truncate();
          p3 = pop();
          push(p2);
          push(p3);
          subtract();
          p4 = pop();
          if (!isZeroAtomOrTensor(p4)) {
            push_symbol(POWER);
            push(p1);
            push(p4);
            list(3);
          }
          push(p3);
          expo = pop_integer();
          if (isNaN(expo)) {
            push_symbol(POWER);
            push(p1);
            push(p3);
            list(3);
            restore();
            return;
          }
          if (expo === 0) {
            restore();
            return;
          }
          push(p1);
          bignum_power_number(expo);
          return restore();
        };
        Eval_quotient = function() {
          push(cadr(p1));
          Eval();
          push(caddr(p1));
          Eval();
          push(cadddr(p1));
          Eval();
          p1 = pop();
          if (p1 === symbol(NIL)) {
            p1 = symbol(SYMBOL_X);
          }
          push(p1);
          return divpoly();
        };
        divpoly = function() {
          var dividend, divisor, h5, i5, l1, m3, n9, ref2, x2;
          h5 = 0;
          i5 = 0;
          m3 = 0;
          n9 = 0;
          x2 = 0;
          save();
          p3 = pop();
          p2 = pop();
          p1 = pop();
          h5 = tos;
          dividend = tos;
          m3 = coeff(p3, p1) - 1;
          divisor = tos;
          n9 = coeff(p3, p2) - 1;
          x2 = m3 - n9;
          push_integer(0);
          p5 = pop();
          while (x2 >= 0) {
            push(stack[dividend + m3]);
            push(stack[divisor + n9]);
            divide();
            p4 = pop();
            for (i5 = l1 = 0, ref2 = n9; 0 <= ref2 ? l1 <= ref2 : l1 >= ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
              push(stack[dividend + x2 + i5]);
              push(stack[divisor + i5]);
              push(p4);
              multiply();
              subtract();
              stack[dividend + x2 + i5] = pop();
            }
            push(p5);
            push(p4);
            push(p3);
            push_integer(x2);
            power();
            multiply();
            add();
            p5 = pop();
            m3--;
            x2--;
          }
          moveTos(h5);
          push(p5);
          return restore();
        };
        DEBUG_RATIONALIZE = false;
        Eval_rationalize = function() {
          push(cadr(p1));
          Eval();
          return rationalize();
        };
        rationalize = function() {
          var x2;
          x2 = expanding;
          yyrationalize();
          return expanding = x2;
        };
        yyrationalize = function() {
          var commonDenominator, eachTerm, theArgument2;
          theArgument2 = pop();
          if (istensor(theArgument2)) {
            __rationalize_tensor(theArgument2);
            return;
          }
          expanding = 0;
          if (car(theArgument2) !== symbol(ADD)) {
            push(theArgument2);
            return;
          }
          if (DEBUG_RATIONALIZE) {
            console.log("rationalize: this is the input expr: " + theArgument2);
          }
          push(one);
          multiply_denominators(theArgument2);
          commonDenominator = pop();
          if (DEBUG_RATIONALIZE) {
            console.log("rationalize: this is the new denominator: " + commonDenominator);
          }
          push(zero);
          eachTerm = cdr(theArgument2);
          while (iscons(eachTerm)) {
            if (DEBUG_RATIONALIZE) {
              console.log("term: " + car(eachTerm));
            }
            push(commonDenominator);
            push(car(eachTerm));
            multiply();
            add();
            eachTerm = cdr(eachTerm);
          }
          if (DEBUG_RATIONALIZE) {
            console.log("rationalize: original terms times new denominator: " + stack[tos - 1]);
          }
          Condense();
          if (DEBUG_RATIONALIZE) {
            console.log("rationalize: after factoring: " + stack[tos - 1]);
          }
          push(commonDenominator);
          divide();
          if (DEBUG_RATIONALIZE) {
            return console.log("rationalize: after dividing by new denom. (and we're done): " + stack[tos - 1]);
          }
        };
        multiply_denominators = function(p11) {
          var results;
          if (car(p11) === symbol(ADD)) {
            p11 = cdr(p11);
            results = [];
            while (iscons(p11)) {
              multiply_denominators_term(car(p11));
              results.push(p11 = cdr(p11));
            }
            return results;
          } else {
            return multiply_denominators_term(p11);
          }
        };
        multiply_denominators_term = function(p11) {
          var results;
          if (car(p11) === symbol(MULTIPLY)) {
            p11 = cdr(p11);
            results = [];
            while (iscons(p11)) {
              multiply_denominators_factor(car(p11));
              results.push(p11 = cdr(p11));
            }
            return results;
          } else {
            return multiply_denominators_factor(p11);
          }
        };
        multiply_denominators_factor = function(p11) {
          if (car(p11) !== symbol(POWER)) {
            return;
          }
          push(p11);
          p11 = caddr(p11);
          if (isnegativenumber(p11)) {
            inverse();
            __lcm();
            return;
          }
          if (car(p11) === symbol(MULTIPLY) && isnegativenumber(cadr(p11))) {
            inverse();
            __lcm();
            return;
          }
          return pop();
        };
        __rationalize_tensor = function(theTensor) {
          var i5, l1, n9, ref2;
          i5 = 0;
          push(theTensor);
          Eval();
          theTensor = pop();
          if (!istensor(theTensor)) {
            push(theTensor);
            return;
          }
          n9 = theTensor.tensor.nelem;
          for (i5 = l1 = 0, ref2 = n9; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
            push(theTensor.tensor.elem[i5]);
            rationalize();
            theTensor.tensor.elem[i5] = pop();
          }
          check_tensor_dimensions(theTensor);
          return push(theTensor);
        };
        __lcm = function() {
          save();
          p1 = pop();
          p2 = pop();
          push(p1);
          push(p2);
          multiply();
          push(p1);
          push(p2);
          gcd();
          divide();
          return restore();
        };
        Eval_real = function() {
          push(cadr(p1));
          Eval();
          return real();
        };
        real = function() {
          save();
          rect();
          p1 = pop();
          push(p1);
          push(p1);
          conjugate();
          add();
          push_integer(2);
          divide();
          return restore();
        };
        DEBUG_RECT = false;
        Eval_rect = function() {
          push(cadr(p1));
          Eval();
          return rect();
        };
        rect = function() {
          var input;
          save();
          p1 = pop();
          input = p1;
          if (DEBUG_RECT) {
            console.log("RECT of " + input);
          }
          if (DEBUG_RECT) {
            console.log("any clock forms in : " + input + " ? " + findPossibleClockForm(input));
          }
          if (issymbol(p1)) {
            if (DEBUG_RECT) {
              console.log(" rect: simple symbol: " + input);
            }
            if (!isZeroAtomOrTensor(get_binding(symbol(ASSUME_REAL_VARIABLES)))) {
              push(p1);
            } else {
              push_symbol(YYRECT);
              push(p1);
              list(2);
            }
          } else if (!isZeroAtomOrTensor(get_binding(symbol(ASSUME_REAL_VARIABLES))) && !findPossibleExponentialForm(p1) && !findPossibleClockForm(p1) && !(Find(p1, symbol(SIN)) && Find(p1, symbol(COS)) && Find(p1, imaginaryunit))) {
            if (DEBUG_RECT) {
              console.log(" rect: simple symbol: " + input);
            }
            push(p1);
          } else if (car(p1) === symbol(MULTIPLY) && isimaginaryunit(cadr(p1)) && !isZeroAtomOrTensor(get_binding(symbol(ASSUME_REAL_VARIABLES)))) {
            push(p1);
          } else if (car(p1) === symbol(ADD)) {
            if (DEBUG_RECT) {
              console.log(" rect - " + input + " is a sum ");
            }
            push_integer(0);
            p1 = cdr(p1);
            while (iscons(p1)) {
              push(car(p1));
              rect();
              add();
              p1 = cdr(p1);
            }
          } else {
            if (DEBUG_RECT) {
              console.log(" rect - " + input + " is NOT a sum ");
            }
            push(p1);
            abs();
            if (DEBUG_RECT) {
              console.log(" rect - " + input + " abs: " + stack[tos - 1].toString());
            }
            push(p1);
            arg();
            if (DEBUG_RECT) {
              console.log(" rect - " + input + " arg of " + p1 + " : " + stack[tos - 1].toString());
            }
            p1 = pop();
            push(p1);
            cosine();
            if (DEBUG_RECT) {
              console.log(" rect - " + input + " cosine: " + stack[tos - 1].toString());
            }
            push(imaginaryunit);
            push(p1);
            sine();
            if (DEBUG_RECT) {
              console.log(" rect - " + input + " sine: " + stack[tos - 1].toString());
            }
            multiply();
            if (DEBUG_RECT) {
              console.log(" rect - " + input + " i * sine: " + stack[tos - 1].toString());
            }
            add();
            if (DEBUG_RECT) {
              console.log(" rect - " + input + " cos + i * sine: " + stack[tos - 1].toString());
            }
            multiply();
          }
          restore();
          if (DEBUG_RECT) {
            return console.log("rect of " + input + " : " + stack[tos - 1]);
          }
        };
        show_power_debug = false;
        performing_roots = false;
        Eval_roots = function() {
          p2 = cadr(p1);
          if (car(p2) === symbol(SETQ) || car(p2) === symbol(TESTEQ)) {
            push(cadr(p2));
            Eval();
            push(caddr(p2));
            Eval();
            subtract();
          } else {
            push(p2);
            Eval();
            p2 = pop();
            if (car(p2) === symbol(SETQ) || car(p2) === symbol(TESTEQ)) {
              push(cadr(p2));
              Eval();
              push(caddr(p2));
              Eval();
              subtract();
            } else {
              push(p2);
            }
          }
          push(caddr(p1));
          Eval();
          p2 = pop();
          if (p2 === symbol(NIL)) {
            guess();
          } else {
            push(p2);
          }
          p2 = pop();
          p1 = pop();
          if (!ispolyexpandedform(p1, p2)) {
            stop("roots: 1st argument is not a polynomial in the variable " + p2);
          }
          push(p1);
          push(p2);
          return roots();
        };
        hasImaginaryCoeff = function(k2) {
          var h5, i5, imaginaryCoefficients, l1, ref2;
          imaginaryCoefficients = false;
          h5 = tos;
          for (i5 = l1 = ref2 = k2; l1 > 0; i5 = l1 += -1) {
            if (iscomplexnumber(stack[tos - i5])) {
              imaginaryCoefficients = true;
              break;
            }
          }
          return imaginaryCoefficients;
        };
        isSimpleRoot = function(k2) {
          var h5, i5, isSimpleRootPolynomial, l1, ref2;
          if (k2 > 2) {
            isSimpleRootPolynomial = true;
            h5 = tos;
            if (isZeroAtomOrTensor(stack[tos - k2])) {
              isSimpleRootPolynomial = false;
            }
            for (i5 = l1 = ref2 = k2 - 1; l1 > 1; i5 = l1 += -1) {
              if (!isZeroAtomOrTensor(stack[tos - i5])) {
                isSimpleRootPolynomial = false;
                break;
              }
            }
          } else {
            isSimpleRootPolynomial = false;
          }
          return isSimpleRootPolynomial;
        };
        normalisedCoeff = function(variable, polynomial) {
          var divideBy, i5, k2, l1, m1, miniStack, ref2, ref3;
          k2 = coeff(variable, polynomial);
          divideBy = stack[tos - 1];
          miniStack = [];
          for (i5 = l1 = 1, ref2 = k2; 1 <= ref2 ? l1 <= ref2 : l1 >= ref2; i5 = 1 <= ref2 ? ++l1 : --l1) {
            miniStack.push(pop());
          }
          for (i5 = m1 = ref3 = k2 - 1; ref3 <= 0 ? m1 <= 0 : m1 >= 0; i5 = ref3 <= 0 ? ++m1 : --m1) {
            push(miniStack[i5]);
            push(divideBy);
            divide();
          }
          return k2;
        };
        roots = function() {
          var h5, i5, k2, l1, lastCoeff, leadingCoeff, n9, ref2;
          h5 = 0;
          i5 = 0;
          n9 = 0;
          if (DEBUG) {
            console.log("roots: " + stack[tos - 1].toString() + " " + stack[tos - 2].toString());
          }
          save();
          if (recursionLevelNestedRadicalsRemoval > 1) {
            pop();
            pop();
            push(symbol(NIL));
            restore();
            return;
          }
          performing_roots = true;
          h5 = tos - 2;
          if (DEBUG) {
            console.log("roots checking if " + stack[tos - 1].toString() + " is a case of simple roots");
          }
          p2 = pop();
          p1 = pop();
          push(p1);
          push(p2);
          k2 = normalisedCoeff(p2, p1);
          if (isSimpleRoot(k2)) {
            if (DEBUG) {
              console.log("yes, " + stack[tos - 1].toString() + " is a case of simple roots");
            }
            lastCoeff = stack[tos - k2];
            leadingCoeff = stack[tos - 1];
            moveTos(tos - k2);
            pop();
            pop();
            getSimpleRoots(k2, leadingCoeff, lastCoeff);
          } else {
            moveTos(tos - k2);
            roots2();
          }
          n9 = tos - h5;
          if (n9 === 0) {
            stop("roots: the polynomial is not factorable, try nroots");
          }
          if (n9 === 1) {
            performing_roots = false;
            restore();
            return;
          }
          sort_stack(n9);
          p1 = alloc_tensor(n9);
          p1.tensor.ndim = 1;
          p1.tensor.dim[0] = n9;
          for (i5 = l1 = 0, ref2 = n9; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
            p1.tensor.elem[i5] = stack[h5 + i5];
          }
          moveTos(h5);
          push(p1);
          restore();
          return performing_roots = false;
        };
        getSimpleRoots = function(n9, leadingCoeff, lastCoeff) {
          var aSol, commonPart, l1, m1, ref2, ref3, rootsOfOne;
          if (DEBUG) {
            console.log("getSimpleRoots");
          }
          save();
          n9 = n9 - 1;
          push(lastCoeff);
          push_rational(1, n9);
          power();
          push(leadingCoeff);
          push_rational(1, n9);
          power();
          divide();
          commonPart = pop();
          if (n9 % 2 === 0) {
            for (rootsOfOne = l1 = 1, ref2 = n9; l1 <= ref2; rootsOfOne = l1 += 2) {
              push(commonPart);
              push_integer(-1);
              push_rational(rootsOfOne, n9);
              power();
              multiply();
              aSol = pop();
              push(aSol);
              push(aSol);
              negate();
            }
          } else {
            for (rootsOfOne = m1 = 1, ref3 = n9; 1 <= ref3 ? m1 <= ref3 : m1 >= ref3; rootsOfOne = 1 <= ref3 ? ++m1 : --m1) {
              push(commonPart);
              push_integer(-1);
              push_rational(rootsOfOne, n9);
              power();
              multiply();
              if (rootsOfOne % 2 === 0) {
                negate();
              }
            }
          }
          return restore();
        };
        roots2 = function() {
          var k2;
          save();
          if (DEBUG) {
            console.log("roots2: " + stack[tos - 1].toString() + " " + stack[tos - 2].toString());
          }
          p2 = pop();
          p1 = pop();
          push(p1);
          push(p2);
          k2 = normalisedCoeff(p2, p1);
          if (!hasImaginaryCoeff(k2)) {
            moveTos(tos - k2);
            factorpoly();
            p1 = pop();
          } else {
            moveTos(tos - k2);
            pop();
            pop();
          }
          if (car(p1) === symbol(MULTIPLY)) {
            p1 = cdr(p1);
            while (iscons(p1)) {
              push(car(p1));
              push(p2);
              roots3();
              p1 = cdr(p1);
            }
          } else {
            push(p1);
            push(p2);
            roots3();
          }
          return restore();
        };
        roots3 = function() {
          var n9;
          save();
          p2 = pop();
          p1 = pop();
          if (car(p1) === symbol(POWER) && ispolyexpandedform(cadr(p1), p2) && isposint(caddr(p1))) {
            n9 = normalisedCoeff(p2, cadr(p1));
            mini_solve(n9);
          } else if (ispolyexpandedform(p1, p2)) {
            n9 = normalisedCoeff(p2, p1);
            mini_solve(n9);
          }
          return restore();
        };
        mini_solve = function(n9) {
          var C_CHECKED_AS_NOT_ZERO, Q_CHECKED_AS_NOT_ZERO, R_18_a_b_c_d, R_27_a2_d, R_2_b3, R_3_a, R_3_a_C, R_3_a_c, R_4_DELTA03, R_6_a, R_6_a_C, R_C, R_C_over_3a, R_C_simplified_toCheckIfZero, R_DELTA0, R_DELTA0_simplified_toCheckIfZero, R_DELTA0_toBeCheckedIfZero, R_DELTA1, R_Q, R_Q_simplified_toCheckIfZero, R_S, R_S_simplified_toCheckIfZero, R_a2, R_a2_d, R_a2_d2, R_a3, R_a_b_c, R_a_b_c_d, R_a_c, R_b2, R_b2_c2, R_b3, R_b3_d, R_c2, R_c3, R_d2, R_determinant, R_determinant_simplified_toCheckIfZero, R_e2, R_e3, R_m, R_m27_a2_d2, R_m4_a_c3, R_m4_b3_d, R_m9_a_b_c, R_m_b_over_3a, R_minus_4S2_minus_2p, R_minus_b_over_4a, R_p, R_principalCubicRoot, R_q, R_q_over_S, R_r, S_CHECKED_AS_NOT_ZERO, ThreePPlus2M, TwoQOversqrtPPlus2M, biquadraticSolutions, choiceOfRadicalInQSoSIsNotZero, coeff2, coeff3, coeff4, depressedSolutions, eachSolution, flipSignOFQSoCIsNotZero, flipSignOFRadicalSoQIsNotZero, i_sqrt3, l1, len, len1, len2, m1, n1, one_minus_i_sqrt3, one_plus_i_sqrt3, ref2, ref3, ref4, resolventCubicSolutions, root_solution, sqrtPPlus2M, toBeCheckedIFZero;
          save();
          if (n9 === 2) {
            p3 = pop();
            p4 = pop();
            push(p4);
            push(p3);
            divide();
            negate();
            restore();
            return;
          }
          if (n9 === 3) {
            p3 = pop();
            p4 = pop();
            p5 = pop();
            push(p4);
            push_integer(2);
            power();
            push_integer(4);
            push(p3);
            multiply();
            push(p5);
            multiply();
            subtract();
            push_rational(1, 2);
            power();
            p6 = pop();
            push(p6);
            push(p4);
            subtract();
            push(p3);
            push_integer(2);
            multiply();
            divide();
            push(p6);
            push(p4);
            add();
            negate();
            push(p3);
            divide();
            push_rational(1, 2);
            multiply();
            restore();
            return;
          }
          if (n9 === 4 || n9 === 5) {
            p3 = pop();
            p4 = pop();
            p5 = pop();
            p6 = pop();
            push(p5);
            push(p5);
            multiply();
            R_c2 = pop();
            push(R_c2);
            push(p5);
            multiply();
            R_c3 = pop();
            push(p4);
            push(p4);
            multiply();
            R_b2 = pop();
            push(R_b2);
            push(p4);
            multiply();
            R_b3 = pop();
            push(R_b3);
            push(p6);
            multiply();
            R_b3_d = pop();
            push(R_b3_d);
            push_integer(-4);
            multiply();
            R_m4_b3_d = pop();
            push(R_b3);
            push_integer(2);
            multiply();
            R_2_b3 = pop();
            push(p3);
            push(p3);
            multiply();
            R_a2 = pop();
            push(R_a2);
            push(p3);
            multiply();
            R_a3 = pop();
            push_integer(3);
            push(p3);
            multiply();
            R_3_a = pop();
            push(R_a2);
            push(p6);
            multiply();
            R_a2_d = pop();
            push(R_a2_d);
            push(p6);
            multiply();
            R_a2_d2 = pop();
            push(R_a2_d);
            push_integer(27);
            multiply();
            R_27_a2_d = pop();
            push(R_a2_d2);
            push_integer(-27);
            multiply();
            R_m27_a2_d2 = pop();
            push(R_3_a);
            push_integer(2);
            multiply();
            R_6_a = pop();
            push(p3);
            push(p5);
            multiply();
            R_a_c = pop();
            push(R_a_c);
            push(p4);
            multiply();
            R_a_b_c = pop();
            push(R_a_b_c);
            push(p6);
            multiply();
            R_a_b_c_d = pop();
            push(R_a_c);
            push_integer(3);
            multiply();
            R_3_a_c = pop();
            push_integer(-4);
            push(p3);
            push(R_c3);
            multiply();
            multiply();
            R_m4_a_c3 = pop();
            push(R_a_b_c);
            push_integer(9);
            multiply();
            negate();
            R_m9_a_b_c = pop();
            push(R_a_b_c_d);
            push_integer(18);
            multiply();
            R_18_a_b_c_d = pop();
            push(R_b2);
            push(R_3_a_c);
            subtract();
            R_DELTA0 = pop();
            push(R_b2);
            push(R_c2);
            multiply();
            R_b2_c2 = pop();
            push(p4);
            negate();
            push(R_3_a);
            divide();
            R_m_b_over_3a = pop();
            if (n9 === 4) {
              if (DEBUG) {
                console.log(">>>>>>>>>>>>>>>> actually using cubic formula <<<<<<<<<<<<<<< ");
              }
              if (DEBUG) {
                console.log("cubic: D0: " + R_DELTA0.toString());
              }
              push(R_DELTA0);
              push_integer(3);
              power();
              push_integer(4);
              multiply();
              R_4_DELTA03 = pop();
              push(R_DELTA0);
              simplify();
              absValFloat();
              R_DELTA0_toBeCheckedIfZero = pop();
              if (DEBUG) {
                console.log("cubic: D0 as float: " + R_DELTA0_toBeCheckedIfZero.toString());
              }
              push(R_18_a_b_c_d);
              push(R_m4_b3_d);
              push(R_b2_c2);
              push(R_m4_a_c3);
              push(R_m27_a2_d2);
              add();
              add();
              add();
              add();
              simplify();
              absValFloat();
              R_determinant = pop();
              if (DEBUG) {
                console.log("cubic: DETERMINANT: " + R_determinant.toString());
              }
              push(R_2_b3);
              push(R_m9_a_b_c);
              push(R_27_a2_d);
              add();
              add();
              R_DELTA1 = pop();
              if (DEBUG) {
                console.log("cubic: D1: " + R_DELTA1.toString());
              }
              push(R_DELTA1);
              push_integer(2);
              power();
              push(R_4_DELTA03);
              subtract();
              push_rational(1, 2);
              power();
              simplify();
              R_Q = pop();
              if (isZeroAtomOrTensor(R_determinant)) {
                if (isZeroAtomOrTensor(R_DELTA0_toBeCheckedIfZero)) {
                  if (DEBUG) {
                    console.log(" cubic: DETERMINANT IS ZERO and delta0 is zero");
                  }
                  push(R_m_b_over_3a);
                  restore();
                  return;
                } else {
                  if (DEBUG) {
                    console.log(" cubic: DETERMINANT IS ZERO and delta0 is not zero");
                  }
                  push(p3);
                  push(p6);
                  push_integer(9);
                  multiply();
                  multiply();
                  push(p4);
                  push(p5);
                  multiply();
                  subtract();
                  push(R_DELTA0);
                  push_integer(2);
                  multiply();
                  divide();
                  root_solution = pop();
                  push(root_solution);
                  push(root_solution);
                  push(R_a_b_c);
                  push_integer(4);
                  multiply();
                  push(p3);
                  push(p3);
                  push(p6);
                  push_integer(9);
                  multiply();
                  multiply();
                  multiply();
                  negate();
                  push(R_b3);
                  negate();
                  add();
                  add();
                  push(p3);
                  push(R_DELTA0);
                  multiply();
                  divide();
                  restore();
                  return;
                }
              }
              C_CHECKED_AS_NOT_ZERO = false;
              flipSignOFQSoCIsNotZero = false;
              while (!C_CHECKED_AS_NOT_ZERO) {
                push(R_Q);
                if (flipSignOFQSoCIsNotZero) {
                  negate();
                }
                push(R_DELTA1);
                add();
                push_rational(1, 2);
                multiply();
                push_rational(1, 3);
                power();
                simplify();
                R_C = pop();
                if (DEBUG) {
                  console.log("cubic: C: " + R_C.toString());
                }
                push(R_C);
                simplify();
                absValFloat();
                R_C_simplified_toCheckIfZero = pop();
                if (DEBUG) {
                  console.log("cubic: C as absval and float: " + R_C_simplified_toCheckIfZero.toString());
                }
                if (isZeroAtomOrTensor(R_C_simplified_toCheckIfZero)) {
                  if (DEBUG) {
                    console.log(" cubic: C IS ZERO flipping the sign");
                  }
                  flipSignOFQSoCIsNotZero = true;
                } else {
                  C_CHECKED_AS_NOT_ZERO = true;
                }
              }
              push(R_C);
              push(R_3_a);
              multiply();
              R_3_a_C = pop();
              push(R_3_a_C);
              push_integer(2);
              multiply();
              R_6_a_C = pop();
              push(imaginaryunit);
              push_integer(3);
              push_rational(1, 2);
              power();
              multiply();
              i_sqrt3 = pop();
              push_integer(1);
              push(i_sqrt3);
              add();
              one_plus_i_sqrt3 = pop();
              push_integer(1);
              push(i_sqrt3);
              subtract();
              one_minus_i_sqrt3 = pop();
              push(R_C);
              push(R_3_a);
              divide();
              R_C_over_3a = pop();
              push(R_m_b_over_3a);
              push(R_C_over_3a);
              negate();
              push(R_DELTA0);
              push(R_3_a_C);
              divide();
              negate();
              add();
              add();
              simplify();
              push(R_m_b_over_3a);
              push(R_C_over_3a);
              push(one_plus_i_sqrt3);
              multiply();
              push_integer(2);
              divide();
              push(one_minus_i_sqrt3);
              push(R_DELTA0);
              multiply();
              push(R_6_a_C);
              divide();
              add();
              add();
              simplify();
              push(R_m_b_over_3a);
              push(R_C_over_3a);
              push(one_minus_i_sqrt3);
              multiply();
              push_integer(2);
              divide();
              push(one_plus_i_sqrt3);
              push(R_DELTA0);
              multiply();
              push(R_6_a_C);
              divide();
              add();
              add();
              simplify();
              restore();
              return;
            }
            if (n9 === 5) {
              if (DEBUG) {
                console.log(">>>>>>>>>>>>>>>> actually using quartic formula <<<<<<<<<<<<<<< ");
              }
              p7 = pop();
              if (isZeroAtomOrTensor(p4) && isZeroAtomOrTensor(p6) && !isZeroAtomOrTensor(p5) && !isZeroAtomOrTensor(p7)) {
                if (DEBUG) {
                  console.log("biquadratic case");
                }
                push(p3);
                push(symbol(SECRETX));
                push_integer(2);
                power();
                multiply();
                push(p5);
                push(symbol(SECRETX));
                multiply();
                push(p7);
                add();
                add();
                push(symbol(SECRETX));
                roots();
                biquadraticSolutions = pop();
                ref2 = biquadraticSolutions.tensor.elem;
                for (l1 = 0, len = ref2.length; l1 < len; l1++) {
                  eachSolution = ref2[l1];
                  push(eachSolution);
                  push_rational(1, 2);
                  power();
                  simplify();
                  push(eachSolution);
                  push_rational(1, 2);
                  power();
                  negate();
                  simplify();
                }
                restore();
                return;
              }
              push(p6);
              push(p6);
              multiply();
              R_d2 = pop();
              push(p7);
              push(p7);
              multiply();
              R_e2 = pop();
              push(R_e2);
              push(p7);
              multiply();
              R_e3 = pop();
              push_integer(256);
              push(R_a3);
              push(R_e3);
              multiply();
              multiply();
              push_integer(-192);
              push(R_a2_d);
              push(R_e2);
              push(p4);
              multiply();
              multiply();
              multiply();
              push_integer(-128);
              push(R_a2);
              push(R_c2);
              push(R_e2);
              multiply();
              multiply();
              multiply();
              push_integer(144);
              push(R_a2_d2);
              push(p5);
              push(p7);
              multiply();
              multiply();
              multiply();
              push(R_m27_a2_d2);
              push(R_d2);
              multiply();
              push_integer(144);
              push(R_a_b_c);
              push(p4);
              push(R_e2);
              multiply();
              multiply();
              multiply();
              push_integer(-6);
              push(p3);
              push(R_b2);
              push(R_d2);
              push(p7);
              multiply();
              multiply();
              multiply();
              multiply();
              push_integer(-80);
              push(R_a_b_c_d);
              push(p5);
              push(p7);
              multiply();
              multiply();
              multiply();
              push_integer(18);
              push(R_a_b_c_d);
              push(R_d2);
              multiply();
              multiply();
              push_integer(16);
              push(R_a_c);
              push(R_c3);
              push(p7);
              multiply();
              multiply();
              multiply();
              push_integer(-4);
              push(R_a_c);
              push(R_c2);
              push(R_d2);
              multiply();
              multiply();
              multiply();
              push_integer(-27);
              push(R_b3);
              push(p4);
              push(R_e2);
              multiply();
              multiply();
              multiply();
              push_integer(18);
              push(R_b3_d);
              push(p5);
              push(p7);
              multiply();
              multiply();
              multiply();
              push(R_m4_b3_d);
              push(R_d2);
              multiply();
              push_integer(-4);
              push(R_b2_c2);
              push(p5);
              push(p7);
              multiply();
              multiply();
              multiply();
              push(R_b2_c2);
              push(R_d2);
              multiply();
              add();
              add();
              add();
              add();
              add();
              add();
              add();
              add();
              add();
              add();
              add();
              add();
              add();
              add();
              add();
              R_determinant = pop();
              if (DEBUG) {
                console.log("R_determinant: " + R_determinant.toString());
              }
              push(R_c2);
              push_integer(-3);
              push(p4);
              push(p6);
              multiply();
              multiply();
              push_integer(12);
              push(p3);
              push(p7);
              multiply();
              multiply();
              add();
              add();
              R_DELTA0 = pop();
              if (DEBUG) {
                console.log("R_DELTA0: " + R_DELTA0.toString());
              }
              push_integer(2);
              push(R_c3);
              multiply();
              push_integer(-9);
              push(p4);
              push(p5);
              push(p6);
              multiply();
              multiply();
              multiply();
              push_integer(27);
              push(R_b2);
              push(p7);
              multiply();
              multiply();
              push_integer(27);
              push(p3);
              push(R_d2);
              multiply();
              multiply();
              push_integer(-72);
              push(R_a_c);
              push(p7);
              multiply();
              multiply();
              add();
              add();
              add();
              add();
              R_DELTA1 = pop();
              if (DEBUG) {
                console.log("R_DELTA1: " + R_DELTA1.toString());
              }
              push_integer(8);
              push(R_a_c);
              multiply();
              push_integer(-3);
              push(R_b2);
              multiply();
              add();
              push_integer(8);
              push(R_a2);
              multiply();
              divide();
              R_p = pop();
              if (DEBUG) {
                console.log("p: " + R_p.toString());
              }
              push(R_b3);
              push_integer(-4);
              push(R_a_b_c);
              multiply();
              push_integer(8);
              push(R_a2_d);
              multiply();
              add();
              add();
              push_integer(8);
              push(R_a3);
              multiply();
              divide();
              R_q = pop();
              if (DEBUG) {
                console.log("q: " + R_q.toString());
              }
              if (DEBUG) {
                console.log("tos 1 " + tos);
              }
              if (!isZeroAtomOrTensor(p4)) {
                if (DEBUG) {
                  console.log("tos 2 " + tos);
                }
                push_integer(8);
                push(p5);
                push(p3);
                multiply();
                multiply();
                push_integer(-3);
                push(p4);
                push_integer(2);
                power();
                multiply();
                add();
                push_integer(8);
                push(p3);
                push_integer(2);
                power();
                multiply();
                divide();
                R_p = pop();
                if (DEBUG) {
                  console.log("p for depressed quartic: " + R_p.toString());
                }
                push(p4);
                push_integer(3);
                power();
                push_integer(-4);
                push(p3);
                push(p4);
                push(p5);
                multiply();
                multiply();
                multiply();
                push_integer(8);
                push(p6);
                push(p3);
                push_integer(2);
                power();
                multiply();
                multiply();
                add();
                add();
                push_integer(8);
                push(p3);
                push_integer(3);
                power();
                multiply();
                divide();
                R_q = pop();
                if (DEBUG) {
                  console.log("q for depressed quartic: " + R_q.toString());
                }
                push(p4);
                push_integer(4);
                power();
                push_integer(-3);
                multiply();
                push_integer(256);
                push(R_a3);
                push(p7);
                multiply();
                multiply();
                push_integer(-64);
                push(R_a2_d);
                push(p4);
                multiply();
                multiply();
                push_integer(16);
                push(R_b2);
                push(p3);
                push(p5);
                multiply();
                multiply();
                multiply();
                add();
                add();
                add();
                push_integer(256);
                push(p3);
                push_integer(4);
                power();
                multiply();
                divide();
                R_r = pop();
                if (DEBUG) {
                  console.log("r for depressed quartic: " + R_r.toString());
                }
                if (DEBUG) {
                  console.log("tos 4 " + tos);
                }
                push(symbol(SECRETX));
                push_integer(4);
                power();
                if (DEBUG) {
                  console.log("4 * x^4: " + stack[tos - 1].toString());
                }
                push(R_p);
                push(symbol(SECRETX));
                push_integer(2);
                power();
                multiply();
                if (DEBUG) {
                  console.log("R_p * x^2: " + stack[tos - 1].toString());
                }
                push(R_q);
                push(symbol(SECRETX));
                multiply();
                if (DEBUG) {
                  console.log("R_q * x: " + stack[tos - 1].toString());
                }
                push(R_r);
                if (DEBUG) {
                  console.log("R_r: " + stack[tos - 1].toString());
                }
                add();
                add();
                add();
                simplify();
                if (DEBUG) {
                  console.log("solving depressed quartic: " + stack[tos - 1].toString());
                }
                push(symbol(SECRETX));
                roots();
                depressedSolutions = pop();
                if (DEBUG) {
                  console.log("depressedSolutions: " + depressedSolutions);
                }
                ref3 = depressedSolutions.tensor.elem;
                for (m1 = 0, len1 = ref3.length; m1 < len1; m1++) {
                  eachSolution = ref3[m1];
                  push(eachSolution);
                  push(p4);
                  push_integer(4);
                  push(p3);
                  multiply();
                  divide();
                  subtract();
                  simplify();
                  if (DEBUG) {
                    console.log("solution from depressed: " + stack[tos - 1].toString());
                  }
                }
                restore();
                return;
              } else {
                R_p = p5;
                R_q = p6;
                R_r = p7;
                push_rational(5, 2);
                push(R_p);
                multiply();
                coeff2 = pop();
                push_integer(2);
                push(R_p);
                push_integer(2);
                power();
                multiply();
                push(R_r);
                subtract();
                coeff3 = pop();
                push(R_p);
                push_integer(3);
                power();
                push_integer(2);
                divide();
                push_rational(-1, 2);
                push(R_p);
                push(R_r);
                multiply();
                multiply();
                push_rational(-1, 8);
                push(R_q);
                push_integer(2);
                power();
                multiply();
                add();
                add();
                coeff4 = pop();
                push(symbol(SECRETX));
                push_integer(3);
                power();
                push(coeff2);
                push(symbol(SECRETX));
                push_integer(2);
                power();
                multiply();
                push(coeff3);
                push(symbol(SECRETX));
                multiply();
                push(coeff4);
                add();
                add();
                add();
                if (DEBUG) {
                  console.log("resolventCubic: " + stack[tos - 1].toString());
                }
                push(symbol(SECRETX));
                roots();
                resolventCubicSolutions = pop();
                if (DEBUG) {
                  console.log("resolventCubicSolutions: " + resolventCubicSolutions);
                }
                R_m = null;
                ref4 = resolventCubicSolutions.tensor.elem;
                for (n1 = 0, len2 = ref4.length; n1 < len2; n1++) {
                  eachSolution = ref4[n1];
                  if (DEBUG) {
                    console.log("examining solution: " + eachSolution);
                  }
                  push(eachSolution);
                  push_integer(2);
                  multiply();
                  push(R_p);
                  add();
                  absValFloat();
                  toBeCheckedIFZero = pop();
                  if (DEBUG) {
                    console.log("abs value is: " + eachSolution);
                  }
                  if (!isZeroAtomOrTensor(toBeCheckedIFZero)) {
                    R_m = eachSolution;
                    break;
                  }
                }
                if (DEBUG) {
                  console.log("chosen solution: " + R_m);
                }
                push(R_m);
                push_integer(2);
                multiply();
                push(R_p);
                add();
                push_rational(1, 2);
                power();
                simplify();
                sqrtPPlus2M = pop();
                push(R_q);
                push_integer(2);
                multiply();
                push(sqrtPPlus2M);
                divide();
                simplify();
                TwoQOversqrtPPlus2M = pop();
                push(R_p);
                push_integer(3);
                multiply();
                push(R_m);
                push_integer(2);
                multiply();
                add();
                ThreePPlus2M = pop();
                push(sqrtPPlus2M);
                push(ThreePPlus2M);
                push(TwoQOversqrtPPlus2M);
                add();
                negate();
                push_rational(1, 2);
                power();
                simplify();
                add();
                push_integer(2);
                divide();
                push(sqrtPPlus2M);
                push(ThreePPlus2M);
                push(TwoQOversqrtPPlus2M);
                add();
                negate();
                push_rational(1, 2);
                power();
                simplify();
                subtract();
                push_integer(2);
                divide();
                push(sqrtPPlus2M);
                negate();
                push(ThreePPlus2M);
                push(TwoQOversqrtPPlus2M);
                subtract();
                negate();
                push_rational(1, 2);
                power();
                simplify();
                add();
                push_integer(2);
                divide();
                push(sqrtPPlus2M);
                negate();
                push(ThreePPlus2M);
                push(TwoQOversqrtPPlus2M);
                subtract();
                negate();
                push_rational(1, 2);
                power();
                simplify();
                subtract();
                push_integer(2);
                divide();
                restore();
                return;
              }
              push(R_determinant);
              simplify();
              absValFloat();
              R_determinant_simplified_toCheckIfZero = pop();
              push(R_DELTA0);
              simplify();
              absValFloat();
              R_DELTA0_simplified_toCheckIfZero = pop();
              S_CHECKED_AS_NOT_ZERO = false;
              choiceOfRadicalInQSoSIsNotZero = 0;
              while (!S_CHECKED_AS_NOT_ZERO) {
                Q_CHECKED_AS_NOT_ZERO = false;
                flipSignOFRadicalSoQIsNotZero = false;
                while (!Q_CHECKED_AS_NOT_ZERO) {
                  push(R_DELTA1);
                  push(R_DELTA1);
                  push_integer(2);
                  power();
                  push_integer(-4);
                  push(R_DELTA0);
                  push_integer(3);
                  power();
                  multiply();
                  add();
                  push_rational(1, 2);
                  power();
                  if (flipSignOFRadicalSoQIsNotZero) {
                    negate();
                  }
                  add();
                  push_integer(2);
                  divide();
                  if (DEBUG) {
                    console.log("content of cubic root: " + stack[tos - 1].toString());
                  }
                  push_rational(1, 3);
                  power();
                  simplify();
                  R_principalCubicRoot = pop();
                  if (DEBUG) {
                    console.log("principal cubic root: " + R_principalCubicRoot.toString());
                  }
                  if (DEBUG) {
                    console.log("tos : " + tos);
                  }
                  if (choiceOfRadicalInQSoSIsNotZero === 0) {
                    if (DEBUG) {
                      console.log("chosing principal cubic root");
                    }
                    push(R_principalCubicRoot);
                  } else if (choiceOfRadicalInQSoSIsNotZero === 1) {
                    if (DEBUG) {
                      console.log("chosing cubic root beyond principal");
                    }
                    push(R_principalCubicRoot);
                    push_rational(-1, 2);
                    multiply();
                    push_integer(3);
                    push_rational(1, 2);
                    power();
                    push(imaginaryunit);
                    multiply();
                    push_rational(-1, 2);
                    multiply();
                    push(R_principalCubicRoot);
                    multiply();
                    add();
                  } else if (choiceOfRadicalInQSoSIsNotZero === 1) {
                    if (DEBUG) {
                      console.log("chosing cubic root beyond beyond principal");
                    }
                    push(R_principalCubicRoot);
                    push_rational(-1, 2);
                    multiply();
                    push_integer(3);
                    push_rational(1, 2);
                    power();
                    push(imaginaryunit);
                    multiply();
                    push_rational(1, 2);
                    multiply();
                    push(R_principalCubicRoot);
                    multiply();
                    add();
                  }
                  simplify();
                  R_Q = pop();
                  if (DEBUG) {
                    console.log("Q " + R_Q.toString());
                  }
                  if (DEBUG) {
                    console.log("tos: " + tos);
                  }
                  push(R_Q);
                  simplify();
                  absValFloat();
                  R_Q_simplified_toCheckIfZero = pop();
                  if (DEBUG) {
                    console.log("Q simplified and abs" + R_Q_simplified_toCheckIfZero.toString());
                  }
                  if (isZeroAtomOrTensor(R_Q_simplified_toCheckIfZero) && (!isZeroAtomOrTensor(R_determinant_simplified_toCheckIfZero) && isZeroAtomOrTensor(R_DELTA0_simplified_toCheckIfZero))) {
                    if (DEBUG) {
                      console.log(" *********************************** Q IS ZERO and it matters, flipping the sign");
                    }
                    flipSignOFRadicalSoQIsNotZero = true;
                  } else {
                    Q_CHECKED_AS_NOT_ZERO = true;
                  }
                  if (DEBUG) {
                    console.log("tos: " + tos);
                  }
                }
                push_rational(-2, 3);
                push(R_p);
                multiply();
                push(R_Q);
                push(R_DELTA0);
                push(R_Q);
                divide();
                add();
                push(R_3_a);
                divide();
                add();
                push_rational(1, 2);
                power();
                push_integer(2);
                divide();
                show_power_debug = true;
                simplify();
                R_S = pop();
                if (DEBUG) {
                  console.log("S " + R_S.toString());
                }
                push(R_S);
                simplify();
                absValFloat();
                R_S_simplified_toCheckIfZero = pop();
                if (DEBUG) {
                  console.log("S " + R_S_simplified_toCheckIfZero.toString());
                }
                if (isZeroAtomOrTensor(R_S_simplified_toCheckIfZero)) {
                  if (DEBUG) {
                    console.log(" *********************************** S IS ZERO chosing another cubic root");
                  }
                  choiceOfRadicalInQSoSIsNotZero++;
                } else {
                  S_CHECKED_AS_NOT_ZERO = true;
                }
                if (DEBUG) {
                  console.log("tos: " + tos);
                }
              }
              if (DEBUG) {
                console.log("tos: " + tos);
              }
              push(p4);
              negate();
              push(p3);
              push_integer(4);
              multiply();
              divide();
              R_minus_b_over_4a = pop();
              push_integer(-4);
              push(R_S);
              push_integer(2);
              power();
              multiply();
              push_integer(2);
              push(R_p);
              multiply();
              subtract();
              R_minus_4S2_minus_2p = pop();
              push(R_q);
              push(R_S);
              divide();
              R_q_over_S = pop();
              if (DEBUG) {
                console.log("tos before putting together the 4 solutions: " + tos);
              }
              push(R_minus_b_over_4a);
              push(R_S);
              subtract();
              push(R_minus_4S2_minus_2p);
              push(R_q_over_S);
              add();
              push_rational(1, 2);
              power();
              push_integer(2);
              divide();
              add();
              simplify();
              push(R_minus_b_over_4a);
              push(R_S);
              subtract();
              push(R_minus_4S2_minus_2p);
              push(R_q_over_S);
              add();
              push_rational(1, 2);
              power();
              push_integer(2);
              divide();
              subtract();
              simplify();
              push(R_minus_b_over_4a);
              push(R_S);
              add();
              push(R_minus_4S2_minus_2p);
              push(R_q_over_S);
              subtract();
              push_rational(1, 2);
              power();
              push_integer(2);
              divide();
              add();
              simplify();
              push(R_minus_b_over_4a);
              push(R_S);
              add();
              push(R_minus_4S2_minus_2p);
              push(R_q_over_S);
              subtract();
              push_rational(1, 2);
              power();
              push_integer(2);
              divide();
              subtract();
              simplify();
              restore();
              return;
            }
          }
          moveTos(tos - n9);
          return restore();
        };
        Eval_round = function() {
          push(cadr(p1));
          Eval();
          return yround();
        };
        yround = function() {
          save();
          yyround();
          return restore();
        };
        yyround = function() {
          var d3;
          d3 = 0;
          p1 = pop();
          if (!isNumericAtom(p1)) {
            push_symbol(ROUND);
            push(p1);
            list(2);
            return;
          }
          if (isdouble(p1)) {
            d3 = Math.round(p1.d);
            push_double(d3);
            return;
          }
          if (isinteger(p1)) {
            push(p1);
            return;
          }
          push(p1);
          yyfloat();
          p1 = pop();
          return push_integer(Math.round(p1.d));
        };
        T_INTEGER = 1001;
        T_DOUBLE = 1002;
        T_SYMBOL = 1003;
        T_FUNCTION = 1004;
        T_NEWLINE = 1006;
        T_STRING = 1007;
        T_GTEQ = 1008;
        T_LTEQ = 1009;
        T_EQ = 1010;
        T_NEQ = 1011;
        T_QUOTASSIGN = 1012;
        token = "";
        newline_flag = 0;
        meta_mode = 0;
        input_str = 0;
        scan_str = 0;
        token_str = 0;
        token_buf = 0;
        lastFoundSymbol = null;
        symbolsRightOfAssignment = null;
        symbolsLeftOfAssignment = null;
        isSymbolLeftOfAssignment = null;
        scanningParameters = null;
        functionInvokationsScanningStack = null;
        skipRootVariableToBeSolved = false;
        assignmentFound = null;
        scanned = "";
        scan = function(s7) {
          if (DEBUG) {
            console.log("#### scanning " + s7);
          }
          lastFoundSymbol = null;
          symbolsRightOfAssignment = [];
          symbolsLeftOfAssignment = [];
          isSymbolLeftOfAssignment = true;
          scanningParameters = [];
          functionInvokationsScanningStack = [""];
          assignmentFound = false;
          scanned = s7;
          meta_mode = 0;
          expanding++;
          input_str = 0;
          scan_str = 0;
          get_next_token();
          if (token === "") {
            push(symbol(NIL));
            expanding--;
            return 0;
          }
          scan_stmt();
          expanding--;
          if (!assignmentFound) {
            symbolsInExpressionsWithoutAssignments = symbolsInExpressionsWithoutAssignments.concat(symbolsLeftOfAssignment);
          }
          return token_str - input_str;
        };
        scan_meta = function(s7) {
          scanned = s7;
          meta_mode = 1;
          expanding++;
          input_str = 0;
          scan_str = 0;
          get_next_token();
          if (token === "") {
            push(symbol(NIL));
            expanding--;
            return 0;
          }
          scan_stmt();
          expanding--;
          return token_str - input_str;
        };
        scan_stmt = function() {
          var assignmentIsOfQuotedType, existingDependencies, i5, indexOfSymbolLeftOfAssignment, l1, len, len1, m1, symbolLeftOfAssignment;
          scan_relation();
          assignmentIsOfQuotedType = false;
          if (token === T_QUOTASSIGN) {
            assignmentIsOfQuotedType = true;
          }
          if (token === T_QUOTASSIGN || token === "=") {
            symbolLeftOfAssignment = lastFoundSymbol;
            if (DEBUG) {
              console.log("assignment!");
            }
            assignmentFound = true;
            isSymbolLeftOfAssignment = false;
            get_next_token();
            push_symbol(SETQ);
            swap();
            if (assignmentIsOfQuotedType) {
              push_symbol(QUOTE);
            }
            scan_relation();
            if (assignmentIsOfQuotedType) {
              list(2);
            }
            list(3);
            isSymbolLeftOfAssignment = true;
            if (codeGen) {
              indexOfSymbolLeftOfAssignment = symbolsRightOfAssignment.indexOf(symbolLeftOfAssignment);
              if (indexOfSymbolLeftOfAssignment !== -1) {
                symbolsRightOfAssignment.splice(indexOfSymbolLeftOfAssignment, 1);
                symbolsHavingReassignments.push(symbolLeftOfAssignment);
              }
              if (DEBUG) {
                console.log("locally, " + symbolLeftOfAssignment + " depends on: ");
                for (l1 = 0, len = symbolsRightOfAssignment.length; l1 < len; l1++) {
                  i5 = symbolsRightOfAssignment[l1];
                  console.log("  " + i5);
                }
              }
              if (symbolsDependencies[symbolLeftOfAssignment] == null) {
                symbolsDependencies[symbolLeftOfAssignment] = [];
              }
              existingDependencies = symbolsDependencies[symbolLeftOfAssignment];
              for (m1 = 0, len1 = symbolsRightOfAssignment.length; m1 < len1; m1++) {
                i5 = symbolsRightOfAssignment[m1];
                if (existingDependencies.indexOf(i5) === -1) {
                  existingDependencies.push(i5);
                }
              }
              return symbolsRightOfAssignment = [];
            }
          }
        };
        scan_relation = function() {
          scan_expression();
          switch (token) {
            case T_EQ:
              push_symbol(TESTEQ);
              swap();
              get_next_token();
              scan_expression();
              return list(3);
            case T_NEQ:
              push_symbol(NOT);
              swap();
              push_symbol(TESTEQ);
              swap();
              get_next_token();
              scan_expression();
              list(3);
              return list(2);
            case T_LTEQ:
              push_symbol(TESTLE);
              swap();
              get_next_token();
              scan_expression();
              return list(3);
            case T_GTEQ:
              push_symbol(TESTGE);
              swap();
              get_next_token();
              scan_expression();
              return list(3);
            case "<":
              push_symbol(TESTLT);
              swap();
              get_next_token();
              scan_expression();
              return list(3);
            case ">":
              push_symbol(TESTGT);
              swap();
              get_next_token();
              scan_expression();
              return list(3);
          }
        };
        scan_expression = function() {
          var h5;
          h5 = tos;
          switch (token) {
            case "+":
              get_next_token();
              scan_term();
              break;
            case "-":
              get_next_token();
              scan_term();
              negate();
              break;
            default:
              scan_term();
          }
          while (newline_flag === 0 && (token === "+" || token === "-")) {
            if (token === "+") {
              get_next_token();
              scan_term();
            } else {
              get_next_token();
              scan_term();
              negate();
            }
          }
          if (tos - h5 > 1) {
            list(tos - h5);
            push_symbol(ADD);
            swap();
            return cons();
          }
        };
        is_factor = function() {
          if ((typeof token.charCodeAt === "function" ? token.charCodeAt(0) : void 0) === dotprod_unicode) {
            return 1;
          }
          switch (token) {
            case "*":
            case "/":
              return 1;
            case "(":
            case T_SYMBOL:
            case T_FUNCTION:
            case T_INTEGER:
            case T_DOUBLE:
            case T_STRING:
              if (newline_flag) {
                scan_str = token_str;
                return 0;
              } else {
                return 1;
              }
          }
          return 0;
        };
        simplify_1_in_products = function(tos2, h5) {
          if (tos2 > h5 && isrational(stack[tos2 - 1]) && equaln(stack[tos2 - 1], 1)) {
            return pop();
          }
        };
        multiply_consecutive_constants = function(tos2, h5) {
          if (tos2 > h5 + 1 && isNumericAtom(stack[tos2 - 2]) && isNumericAtom(stack[tos2 - 1])) {
            return multiply();
          }
        };
        scan_term = function() {
          var h5;
          h5 = tos;
          scan_factor();
          if (parse_time_simplifications) {
            simplify_1_in_products(tos, h5);
          }
          while (is_factor()) {
            if (token === "*") {
              get_next_token();
              scan_factor();
            } else if (token === "/") {
              simplify_1_in_products(tos, h5);
              get_next_token();
              scan_factor();
              inverse();
            } else if ((typeof token.charCodeAt === "function" ? token.charCodeAt(0) : void 0) === dotprod_unicode) {
              get_next_token();
              push_symbol(INNER);
              swap();
              scan_factor();
              list(3);
            } else {
              scan_factor();
            }
            if (parse_time_simplifications) {
              multiply_consecutive_constants(tos, h5);
              simplify_1_in_products(tos, h5);
            }
          }
          if (h5 === tos) {
            return push_integer(1);
          } else if (tos - h5 > 1) {
            list(tos - h5);
            push_symbol(MULTIPLY);
            swap();
            return cons();
          }
        };
        scan_power = function() {
          if (token === "^") {
            get_next_token();
            push_symbol(POWER);
            swap();
            scan_factor();
            return list(3);
          }
        };
        scan_index = function(h5) {
          get_next_token();
          push_symbol(INDEX);
          swap();
          scan_expression();
          while (token === ",") {
            get_next_token();
            scan_expression();
          }
          if (token !== "]") {
            scan_error("] expected");
          }
          get_next_token();
          return list(tos - h5);
        };
        scan_factor = function() {
          var firstFactorIsNumber, h5;
          h5 = tos;
          firstFactorIsNumber = false;
          if (token === "(") {
            scan_subexpr();
          } else if (token === T_SYMBOL) {
            scan_symbol();
          } else if (token === T_FUNCTION) {
            scan_function_call_with_function_name();
          } else if (token === "[") {
            scan_tensor();
          } else if (token === T_INTEGER) {
            firstFactorIsNumber = true;
            bignum_scan_integer(token_buf);
            get_next_token();
          } else if (token === T_DOUBLE) {
            firstFactorIsNumber = true;
            bignum_scan_float(token_buf);
            get_next_token();
          } else if (token === T_STRING) {
            scan_string();
          } else {
            scan_error("syntax error");
          }
          while (token === "[" || token === "(" && newline_flag === 0 && !firstFactorIsNumber) {
            if (token === "[") {
              scan_index(h5);
            } else if (token === "(") {
              scan_function_call_without_function_name();
            }
          }
          while (token === "!") {
            get_next_token();
            push_symbol(FACTORIAL);
            swap();
            list(2);
          }
          while ((typeof token.charCodeAt === "function" ? token.charCodeAt(0) : void 0) === transpose_unicode) {
            get_next_token();
            push_symbol(TRANSPOSE);
            swap();
            list(2);
          }
          return scan_power();
        };
        addSymbolRightOfAssignment = function(theSymbol) {
          var i5, l1, prefixVar, ref2;
          if (predefinedSymbolsInGlobalScope_doNotTrackInDependencies.indexOf(theSymbol) === -1 && symbolsRightOfAssignment.indexOf(theSymbol) === -1 && symbolsRightOfAssignment.indexOf("'" + theSymbol) === -1 && !skipRootVariableToBeSolved) {
            if (DEBUG) {
              console.log("... adding symbol: " + theSymbol + " to the set of the symbols right of assignment");
            }
            prefixVar = "";
            for (i5 = l1 = 1, ref2 = functionInvokationsScanningStack.length; 1 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 1 <= ref2 ? ++l1 : --l1) {
              if (functionInvokationsScanningStack[i5] !== "") {
                prefixVar += functionInvokationsScanningStack[i5] + "_" + i5 + "_";
              }
            }
            theSymbol = prefixVar + theSymbol;
            return symbolsRightOfAssignment.push(theSymbol);
          }
        };
        addSymbolLeftOfAssignment = function(theSymbol) {
          var i5, l1, prefixVar, ref2;
          if (predefinedSymbolsInGlobalScope_doNotTrackInDependencies.indexOf(theSymbol) === -1 && symbolsLeftOfAssignment.indexOf(theSymbol) === -1 && symbolsLeftOfAssignment.indexOf("'" + theSymbol) === -1 && !skipRootVariableToBeSolved) {
            if (DEBUG) {
              console.log("... adding symbol: " + theSymbol + " to the set of the symbols left of assignment");
            }
            prefixVar = "";
            for (i5 = l1 = 1, ref2 = functionInvokationsScanningStack.length; 1 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 1 <= ref2 ? ++l1 : --l1) {
              if (functionInvokationsScanningStack[i5] !== "") {
                prefixVar += functionInvokationsScanningStack[i5] + "_" + i5 + "_";
              }
            }
            theSymbol = prefixVar + theSymbol;
            return symbolsLeftOfAssignment.push(theSymbol);
          }
        };
        scan_symbol = function() {
          if (token !== T_SYMBOL) {
            scan_error("symbol expected");
          }
          if (meta_mode && token_buf.length === 1) {
            switch (token_buf[0]) {
              case "a":
                push(symbol(METAA));
                break;
              case "b":
                push(symbol(METAB));
                break;
              case "x":
                push(symbol(METAX));
                break;
              default:
                push(usr_symbol(token_buf));
            }
          } else {
            push(usr_symbol(token_buf));
          }
          if (scanningParameters.length === 0) {
            if (DEBUG) {
              console.log("out of scanning parameters, processing " + token_buf);
            }
            lastFoundSymbol = token_buf;
            if (isSymbolLeftOfAssignment) {
              addSymbolLeftOfAssignment(token_buf);
            }
          } else {
            if (DEBUG) {
              console.log("still scanning parameters, skipping " + token_buf);
            }
            if (isSymbolLeftOfAssignment) {
              addSymbolRightOfAssignment("'" + token_buf);
            }
          }
          if (DEBUG) {
            console.log("found symbol: " + token_buf + " left of assignment: " + isSymbolLeftOfAssignment);
          }
          if (!isSymbolLeftOfAssignment) {
            addSymbolRightOfAssignment(token_buf);
          }
          return get_next_token();
        };
        scan_string = function() {
          push(new_string(token_buf));
          return get_next_token();
        };
        scan_function_call_with_function_name = function() {
          var functionName, i5, l1, n9, p11, ref2;
          if (DEBUG) {
            console.log("-- scan_function_call_with_function_name start");
          }
          n9 = 1;
          p11 = new U();
          p11 = usr_symbol(token_buf);
          push(p11);
          functionName = token_buf;
          if (functionName === "roots" || functionName === "defint" || functionName === "sum" || functionName === "product" || functionName === "for") {
            functionInvokationsScanningStack.push(token_buf);
          }
          lastFoundSymbol = token_buf;
          if (!isSymbolLeftOfAssignment) {
            addSymbolRightOfAssignment(token_buf);
          }
          get_next_token();
          get_next_token();
          scanningParameters.push(true);
          if (token !== ")") {
            scan_stmt();
            n9++;
            while (token === ",") {
              get_next_token();
              if (n9 === 2 && functionInvokationsScanningStack[functionInvokationsScanningStack.length - 1].indexOf("roots") !== -1) {
                symbolsRightOfAssignment = symbolsRightOfAssignment.filter(function(x2) {
                  return !new RegExp("roots_" + (functionInvokationsScanningStack.length - 1) + "_" + token_buf).test(x2);
                });
                skipRootVariableToBeSolved = true;
              }
              if (n9 === 2 && functionInvokationsScanningStack[functionInvokationsScanningStack.length - 1].indexOf("sum") !== -1) {
                symbolsRightOfAssignment = symbolsRightOfAssignment.filter(function(x2) {
                  return !new RegExp("sum_" + (functionInvokationsScanningStack.length - 1) + "_" + token_buf).test(x2);
                });
                skipRootVariableToBeSolved = true;
              }
              if (n9 === 2 && functionInvokationsScanningStack[functionInvokationsScanningStack.length - 1].indexOf("product") !== -1) {
                symbolsRightOfAssignment = symbolsRightOfAssignment.filter(function(x2) {
                  return !new RegExp("product_" + (functionInvokationsScanningStack.length - 1) + "_" + token_buf).test(x2);
                });
                skipRootVariableToBeSolved = true;
              }
              if (n9 === 2 && functionInvokationsScanningStack[functionInvokationsScanningStack.length - 1].indexOf("for") !== -1) {
                symbolsRightOfAssignment = symbolsRightOfAssignment.filter(function(x2) {
                  return !new RegExp("for_" + (functionInvokationsScanningStack.length - 1) + "_" + token_buf).test(x2);
                });
                skipRootVariableToBeSolved = true;
              }
              if (functionInvokationsScanningStack[functionInvokationsScanningStack.length - 1].indexOf("defint") !== -1 && (n9 === 2 || n9 > 2 && (n9 - 2) % 3 === 0)) {
                symbolsRightOfAssignment = symbolsRightOfAssignment.filter(function(x2) {
                  return !new RegExp("defint_" + (functionInvokationsScanningStack.length - 1) + "_" + token_buf).test(x2);
                });
                skipRootVariableToBeSolved = true;
              }
              scan_stmt();
              skipRootVariableToBeSolved = false;
              n9++;
            }
            if (n9 === 2 && functionInvokationsScanningStack[functionInvokationsScanningStack.length - 1].indexOf("roots") !== -1) {
              symbolsRightOfAssignment = symbolsRightOfAssignment.filter(function(x2) {
                return !new RegExp("roots_" + (functionInvokationsScanningStack.length - 1) + "_x").test(x2);
              });
            }
          }
          scanningParameters.pop();
          for (i5 = l1 = 0, ref2 = symbolsRightOfAssignment.length; 0 <= ref2 ? l1 <= ref2 : l1 >= ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
            if (symbolsRightOfAssignment[i5] != null) {
              if (functionName === "roots") {
                symbolsRightOfAssignment[i5] = symbolsRightOfAssignment[i5].replace(new RegExp("roots_" + (functionInvokationsScanningStack.length - 1) + "_"), "");
              }
              if (functionName === "defint") {
                symbolsRightOfAssignment[i5] = symbolsRightOfAssignment[i5].replace(new RegExp("defint_" + (functionInvokationsScanningStack.length - 1) + "_"), "");
              }
              if (functionName === "sum") {
                symbolsRightOfAssignment[i5] = symbolsRightOfAssignment[i5].replace(new RegExp("sum_" + (functionInvokationsScanningStack.length - 1) + "_"), "");
              }
              if (functionName === "product") {
                symbolsRightOfAssignment[i5] = symbolsRightOfAssignment[i5].replace(new RegExp("product_" + (functionInvokationsScanningStack.length - 1) + "_"), "");
              }
              if (functionName === "for") {
                symbolsRightOfAssignment[i5] = symbolsRightOfAssignment[i5].replace(new RegExp("for_" + (functionInvokationsScanningStack.length - 1) + "_"), "");
              }
            }
          }
          if (token !== ")") {
            scan_error(") expected");
          }
          get_next_token();
          list(n9);
          if (functionName === "roots" || functionName === "defint" || functionName === "sum" || functionName === "product" || functionName === "for") {
            functionInvokationsScanningStack.pop();
          }
          if (functionName === symbol(PATTERN).printname) {
            patternHasBeenFound = true;
          }
          if (DEBUG) {
            return console.log("-- scan_function_call_with_function_name end");
          }
        };
        scan_function_call_without_function_name = function() {
          var n9;
          if (DEBUG) {
            console.log("-- scan_function_call_without_function_name start");
          }
          push_symbol(EVAL);
          swap();
          list(2);
          n9 = 1;
          get_next_token();
          scanningParameters.push(true);
          if (token !== ")") {
            scan_stmt();
            n9++;
            while (token === ",") {
              get_next_token();
              scan_stmt();
              n9++;
            }
          }
          scanningParameters.pop();
          if (token !== ")") {
            scan_error(") expected");
          }
          get_next_token();
          list(n9);
          if (DEBUG) {
            return console.log("-- scan_function_call_without_function_name end: " + stack[tos - 1]);
          }
        };
        scan_subexpr = function() {
          var n9;
          n9 = 0;
          if (token !== "(") {
            scan_error("( expected");
          }
          get_next_token();
          scan_stmt();
          if (token !== ")") {
            scan_error(") expected");
          }
          return get_next_token();
        };
        scan_tensor = function() {
          var n9;
          n9 = 0;
          if (token !== "[") {
            scan_error("[ expected");
          }
          get_next_token();
          scan_stmt();
          n9 = 1;
          while (token === ",") {
            get_next_token();
            scan_stmt();
            n9++;
          }
          build_tensor(n9);
          if (token !== "]") {
            scan_error("] expected");
          }
          return get_next_token();
        };
        scan_error = function(errmsg) {
          errorMessage = "";
          while (input_str !== scan_str) {
            if ((scanned[input_str] === "\n" || scanned[input_str] === "\r") && input_str + 1 === scan_str) {
              break;
            }
            errorMessage += scanned[input_str++];
          }
          errorMessage += " ? ";
          while (scanned[input_str] && (scanned[input_str] !== "\n" && scanned[input_str] !== "\r")) {
            errorMessage += scanned[input_str++];
          }
          errorMessage += "\n";
          return stop(errmsg);
        };
        build_tensor = function(n9) {
          var i5, l1, ref2;
          i5 = 0;
          save();
          p2 = alloc_tensor(n9);
          p2.tensor.ndim = 1;
          p2.tensor.dim[0] = n9;
          for (i5 = l1 = 0, ref2 = n9; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
            p2.tensor.elem[i5] = stack[tos - n9 + i5];
          }
          check_tensor_dimensions(p2);
          moveTos(tos - n9);
          push(p2);
          return restore();
        };
        get_next_token = function() {
          newline_flag = 0;
          while (1) {
            get_token();
            if (token !== T_NEWLINE) {
              break;
            }
            newline_flag = 1;
          }
          if (DEBUG) {
            return console.log("get_next_token token: " + token);
          }
        };
        get_token = function() {
          while (isspace(scanned[scan_str])) {
            if (scanned[scan_str] === "\n" || scanned[scan_str] === "\r") {
              token = T_NEWLINE;
              scan_str++;
              return;
            }
            scan_str++;
          }
          token_str = scan_str;
          if (scan_str === scanned.length) {
            token = "";
            return;
          }
          if (isdigit(scanned[scan_str]) || scanned[scan_str] === ".") {
            while (isdigit(scanned[scan_str])) {
              scan_str++;
            }
            if (scanned[scan_str] === ".") {
              scan_str++;
              while (isdigit(scanned[scan_str])) {
                scan_str++;
              }
              if (scanned[scan_str] === "e" && (scanned[scan_str + 1] === "+" || scanned[scan_str + 1] === "-" || isdigit(scanned[scan_str + 1]))) {
                scan_str += 2;
                while (isdigit(scanned[scan_str])) {
                  scan_str++;
                }
              }
              token = T_DOUBLE;
            } else {
              token = T_INTEGER;
            }
            update_token_buf(token_str, scan_str);
            return;
          }
          if (isalpha(scanned[scan_str])) {
            while (isalnumorunderscore(scanned[scan_str])) {
              scan_str++;
            }
            if (scanned[scan_str] === "(") {
              token = T_FUNCTION;
            } else {
              token = T_SYMBOL;
            }
            update_token_buf(token_str, scan_str);
            return;
          }
          if (scanned[scan_str] === '"') {
            scan_str++;
            while (scanned[scan_str] !== '"') {
              if (scan_str === scanned.length - 1) {
                scan_str++;
                scan_error("runaway string");
                scan_str--;
              }
              scan_str++;
            }
            scan_str++;
            token = T_STRING;
            update_token_buf(token_str + 1, scan_str - 1);
            return;
          }
          if (scanned[scan_str] === "#" || scanned[scan_str] === "-" && scanned[scan_str + 1] === "-") {
            while (scanned[scan_str] && scanned[scan_str] !== "\n" && scanned[scan_str] !== "\r") {
              scan_str++;
            }
            if (scanned[scan_str]) {
              scan_str++;
            }
            token = T_NEWLINE;
            return;
          }
          if (scanned[scan_str] === ":" && scanned[scan_str + 1] === "=") {
            scan_str += 2;
            token = T_QUOTASSIGN;
            return;
          }
          if (scanned[scan_str] === "=" && scanned[scan_str + 1] === "=") {
            scan_str += 2;
            token = T_EQ;
            return;
          }
          if (scanned[scan_str] === "!" && scanned[scan_str + 1] === "=") {
            scan_str += 2;
            token = T_NEQ;
            return;
          }
          if (scanned[scan_str] === "<" && scanned[scan_str + 1] === "=") {
            scan_str += 2;
            token = T_LTEQ;
            return;
          }
          if (scanned[scan_str] === ">" && scanned[scan_str + 1] === "=") {
            scan_str += 2;
            token = T_GTEQ;
            return;
          }
          return token = scanned[scan_str++];
        };
        update_token_buf = function(a4, b2) {
          return token_buf = scanned.substring(a4, b2);
        };
        $.scan = scan;
        Eval_sgn = function() {
          push(cadr(p1));
          Eval();
          return sgn();
        };
        sgn = function() {
          save();
          yysgn();
          return restore();
        };
        yysgn = function() {
          p1 = pop();
          if (isdouble(p1)) {
            if (p1.d > 0) {
              push_integer(1);
              return;
            } else {
              if (p1.d === 0) {
                push_integer(1);
                return;
              } else {
                push_integer(-1);
                return;
              }
            }
          }
          if (isrational(p1)) {
            if (MSIGN(mmul(p1.q.a, p1.q.b)) === -1) {
              push_integer(-1);
              return;
            } else {
              if (MZERO(mmul(p1.q.a, p1.q.b))) {
                push_integer(0);
                return;
              } else {
                push_integer(1);
                return;
              }
            }
          }
          if (iscomplexnumber(p1)) {
            push_integer(-1);
            push(p1);
            absval();
            power();
            push(p1);
            multiply();
            return;
          }
          if (isnegativeterm(p1)) {
            push_symbol(SGN);
            push(p1);
            negate();
            list(2);
            push_integer(-1);
            multiply();
            return;
          }
          push_symbol(SGN);
          push(p1);
          return list(2);
        };
        Eval_shape = function() {
          push(cadr(p1));
          Eval();
          return shape();
        };
        shape = function() {
          var ai, an, i5, l1, m1, ndim, ref2, ref3, t5;
          i5 = 0;
          ndim = 0;
          t5 = 0;
          ai = [];
          an = [];
          for (i5 = l1 = 0, ref2 = MAXDIM; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
            ai[i5] = 0;
            an[i5] = 0;
          }
          save();
          p1 = pop();
          if (!istensor(p1)) {
            if (!isZeroAtomOrTensor(p1)) {
              stop("transpose: tensor expected, 1st arg is not a tensor");
            }
            push(zero);
            restore();
            return;
          }
          ndim = p1.tensor.ndim;
          p2 = alloc_tensor(ndim);
          p2.tensor.ndim = 1;
          p2.tensor.dim[0] = ndim;
          for (i5 = m1 = 0, ref3 = ndim; 0 <= ref3 ? m1 < ref3 : m1 > ref3; i5 = 0 <= ref3 ? ++m1 : --m1) {
            push_integer(p1.tensor.dim[i5]);
            p2.tensor.elem[i5] = pop();
          }
          push(p2);
          return restore();
        };
        Eval_simfac = function() {
          push(cadr(p1));
          Eval();
          return simfac();
        };
        simfac = function() {
          var h5;
          h5 = 0;
          save();
          p1 = pop();
          if (car(p1) === symbol(ADD)) {
            h5 = tos;
            p1 = cdr(p1);
            while (p1 !== symbol(NIL)) {
              push(car(p1));
              simfac_term();
              p1 = cdr(p1);
            }
            add_all(tos - h5);
          } else {
            push(p1);
            simfac_term();
          }
          return restore();
        };
        simfac_term = function() {
          var doNothing, h5;
          h5 = 0;
          save();
          p1 = pop();
          if (car(p1) !== symbol(MULTIPLY)) {
            push(p1);
            restore();
            return;
          }
          h5 = tos;
          p1 = cdr(p1);
          while (p1 !== symbol(NIL)) {
            push(car(p1));
            p1 = cdr(p1);
          }
          while (yysimfac(h5)) {
            doNothing = 1;
          }
          multiply_all_noexpand(tos - h5);
          return restore();
        };
        yysimfac = function(h5) {
          var i5, j2, l1, m1, ref2, ref3, ref4, ref5;
          i5 = 0;
          j2 = 0;
          for (i5 = l1 = ref2 = h5, ref3 = tos; ref2 <= ref3 ? l1 < ref3 : l1 > ref3; i5 = ref2 <= ref3 ? ++l1 : --l1) {
            p1 = stack[i5];
            for (j2 = m1 = ref4 = h5, ref5 = tos; ref4 <= ref5 ? m1 < ref5 : m1 > ref5; j2 = ref4 <= ref5 ? ++m1 : --m1) {
              if (i5 === j2) {
                continue;
              }
              p2 = stack[j2];
              if (car(p1) === symbol(FACTORIAL) && car(p2) === symbol(POWER) && isminusone(caddr(p2)) && equal(cadr(p1), cadr(p2))) {
                push(cadr(p1));
                push(one);
                subtract();
                factorial();
                stack[i5] = pop();
                stack[j2] = one;
                return 1;
              }
              if (car(p2) === symbol(POWER) && isminusone(caddr(p2)) && caadr(p2) === symbol(FACTORIAL) && equal(p1, cadadr(p2))) {
                push(p1);
                push_integer(-1);
                add();
                factorial();
                reciprocate();
                stack[i5] = pop();
                stack[j2] = one;
                return 1;
              }
              if (car(p2) === symbol(FACTORIAL)) {
                push(p1);
                push(cadr(p2));
                subtract();
                p3 = pop();
                if (isplusone(p3)) {
                  push(p1);
                  factorial();
                  stack[i5] = pop();
                  stack[j2] = one;
                  return 1;
                }
              }
              if (car(p1) === symbol(POWER) && isminusone(caddr(p1)) && car(p2) === symbol(POWER) && isminusone(caddr(p2)) && caadr(p2) === symbol(FACTORIAL)) {
                push(cadr(p1));
                push(cadr(cadr(p2)));
                subtract();
                p3 = pop();
                if (isplusone(p3)) {
                  push(cadr(p1));
                  factorial();
                  reciprocate();
                  stack[i5] = pop();
                  stack[j2] = one;
                  return 1;
                }
              }
              if (car(p1) === symbol(FACTORIAL) && car(p2) === symbol(POWER) && isminusone(caddr(p2)) && caadr(p2) === symbol(FACTORIAL)) {
                push(cadr(p1));
                push(cadr(cadr(p2)));
                subtract();
                p3 = pop();
                if (isplusone(p3)) {
                  stack[i5] = cadr(p1);
                  stack[j2] = one;
                  return 1;
                }
                if (isminusone(p3)) {
                  push(cadr(cadr(p2)));
                  reciprocate();
                  stack[i5] = pop();
                  stack[j2] = one;
                  return 1;
                }
                if (equaln(p3, 2)) {
                  stack[i5] = cadr(p1);
                  push(cadr(p1));
                  push_integer(-1);
                  add();
                  stack[j2] = pop();
                  return 1;
                }
                if (equaln(p3, -2)) {
                  push(cadr(cadr(p2)));
                  reciprocate();
                  stack[i5] = pop();
                  push(cadr(cadr(p2)));
                  push_integer(-1);
                  add();
                  reciprocate();
                  stack[j2] = pop();
                  return 1;
                }
              }
            }
          }
          return 0;
        };
        DEBUG_SIMPLIFY = false;
        Eval_simplify = function() {
          push(cadr(p1));
          runUserDefinedSimplifications();
          Eval();
          return simplify();
        };
        runUserDefinedSimplifications = function() {
          var atLeastOneSuccessInRouldOfRulesApplications, eachConsecutiveRuleApplication, eachSimplification, l1, len, len1, m1, numberOfRulesApplications, originalexpanding, success;
          if (userSimplificationsInListForm.length !== 0 && !Find(cadr(p1), symbol(INTEGRAL))) {
            originalexpanding = expanding;
            expanding = false;
            if (DEBUG_SIMPLIFY) {
              console.log("runUserDefinedSimplifications passed: " + stack[tos - 1].toString());
            }
            Eval();
            if (DEBUG_SIMPLIFY) {
              console.log("runUserDefinedSimplifications after eval no expanding: " + stack[tos - 1].toString());
            }
            expanding = originalexpanding;
            p1 = stack[tos - 1];
            if (DEBUG_SIMPLIFY) {
              console.log("patterns to be checked: ");
            }
            for (l1 = 0, len = userSimplificationsInListForm.length; l1 < len; l1++) {
              eachSimplification = userSimplificationsInListForm[l1];
              if (DEBUG_SIMPLIFY) {
                console.log("..." + eachSimplification);
              }
            }
            atLeastOneSuccessInRouldOfRulesApplications = true;
            numberOfRulesApplications = 0;
            while (atLeastOneSuccessInRouldOfRulesApplications && numberOfRulesApplications < MAX_CONSECUTIVE_APPLICATIONS_OF_ALL_RULES) {
              atLeastOneSuccessInRouldOfRulesApplications = false;
              numberOfRulesApplications++;
              for (m1 = 0, len1 = userSimplificationsInListForm.length; m1 < len1; m1++) {
                eachSimplification = userSimplificationsInListForm[m1];
                success = true;
                eachConsecutiveRuleApplication = 0;
                while (success && eachConsecutiveRuleApplication < MAX_CONSECUTIVE_APPLICATIONS_OF_SINGLE_RULE) {
                  eachConsecutiveRuleApplication++;
                  if (DEBUG_SIMPLIFY) {
                    console.log("simplify - tos: " + tos + " checking pattern: " + eachSimplification + " on: " + p1);
                  }
                  push_symbol(NIL);
                  success = transform(eachSimplification, true);
                  if (success) {
                    atLeastOneSuccessInRouldOfRulesApplications = true;
                  }
                  p1 = stack[tos - 1];
                  if (DEBUG_SIMPLIFY) {
                    console.log("p1 at this stage of simplification: " + p1);
                  }
                }
                if (eachConsecutiveRuleApplication === MAX_CONSECUTIVE_APPLICATIONS_OF_SINGLE_RULE) {
                  stop("maximum application of single transformation rule exceeded: " + eachSimplification);
                }
              }
            }
            if (numberOfRulesApplications === MAX_CONSECUTIVE_APPLICATIONS_OF_ALL_RULES) {
              stop("maximum application of all transformation rules exceeded ");
            }
            if (DEBUG_SIMPLIFY) {
              console.log("METAX = " + get_binding(symbol(METAX)));
            }
            if (DEBUG_SIMPLIFY) {
              console.log("METAA = " + get_binding(symbol(METAA)));
            }
            if (DEBUG_SIMPLIFY) {
              return console.log("METAB = " + get_binding(symbol(METAB)));
            }
          }
        };
        simplifyForCodeGeneration = function() {
          save();
          runUserDefinedSimplifications();
          codeGen = true;
          simplify_main();
          codeGen = false;
          return restore();
        };
        simplify = function() {
          save();
          simplify_main();
          return restore();
        };
        simplify_main = function() {
          var args, fbody;
          p1 = pop();
          if (codeGen && car(p1) === symbol(FUNCTION)) {
            fbody = cadr(p1);
            push(fbody);
            eval();
            simplify();
            p3 = pop();
            args = caddr(p1);
            push_symbol(FUNCTION);
            push(p3);
            push(args);
            list(3);
            p1 = pop();
          }
          if (istensor(p1)) {
            simplify_tensor();
            return;
          }
          if (Find(p1, symbol(FACTORIAL))) {
            push(p1);
            simfac();
            p2 = pop();
            push(p1);
            rationalize();
            simfac();
            p3 = pop();
            if (count(p2) < count(p3)) {
              p1 = p2;
            } else {
              p1 = p3;
            }
          }
          f10();
          if (DEBUG_SIMPLIFY) {
            console.log("f10: " + p1.toString());
          }
          f1();
          if (DEBUG_SIMPLIFY) {
            console.log("f1: " + p1.toString());
          }
          f2();
          if (DEBUG_SIMPLIFY) {
            console.log("f2: " + p1.toString());
          }
          f3();
          if (DEBUG_SIMPLIFY) {
            console.log("f3: " + p1.toString());
          }
          f4();
          if (DEBUG_SIMPLIFY) {
            console.log("f4: " + p1.toString());
          }
          f5();
          if (DEBUG_SIMPLIFY) {
            console.log("f5: " + p1.toString());
          }
          f9();
          if (DEBUG_SIMPLIFY) {
            console.log("f9: " + p1.toString());
          }
          simplify_polarRect();
          if (do_simplify_nested_radicals) {
            if (simplify_nested_radicals()) {
              if (DEBUG_SIMPLIFY) {
                console.log("de-nesting successful into: " + p1.toString());
              }
              push(p1);
              simplify();
              return;
            }
          }
          simplify_rectToClock();
          simplify_rational_expressions();
          return push(p1);
        };
        simplify_tensor = function() {
          var i5, l1, m1, ref2, ref3;
          i5 = 0;
          p2 = alloc_tensor(p1.tensor.nelem);
          p2.tensor.ndim = p1.tensor.ndim;
          for (i5 = l1 = 0, ref2 = p1.tensor.ndim; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
            p2.tensor.dim[i5] = p1.tensor.dim[i5];
          }
          for (i5 = m1 = 0, ref3 = p1.tensor.nelem; 0 <= ref3 ? m1 < ref3 : m1 > ref3; i5 = 0 <= ref3 ? ++m1 : --m1) {
            push(p1.tensor.elem[i5]);
            simplify();
            p2.tensor.elem[i5] = pop();
          }
          check_tensor_dimensions(p2);
          if (isZeroAtomOrTensor(p2)) {
            p2 = zero;
          }
          return push(p2);
        };
        f1 = function() {
          if (car(p1) !== symbol(ADD)) {
            return;
          }
          push(p1);
          rationalize();
          p2 = pop();
          if (count(p2) < count(p1)) {
            return p1 = p2;
          }
        };
        f2 = function() {
          if (car(p1) !== symbol(ADD)) {
            return;
          }
          push(p1);
          Condense();
          p2 = pop();
          if (count(p2) <= count(p1)) {
            return p1 = p2;
          }
        };
        f3 = function() {
          push(p1);
          rationalize();
          negate();
          rationalize();
          negate();
          rationalize();
          p2 = pop();
          if (count(p2) < count(p1)) {
            return p1 = p2;
          }
        };
        f10 = function() {
          var a4, b2, carp1, miao, originalexpanding;
          carp1 = car(p1);
          miao = cdr(p1);
          if (carp1 === symbol(MULTIPLY) || isinnerordot(p1)) {
            if (car(car(cdr(p1))) === symbol(TRANSPOSE) && car(car(cdr(cdr(p1)))) === symbol(TRANSPOSE)) {
              if (DEBUG_SIMPLIFY) {
                console.log("maybe collecting a transpose " + p1);
              }
              a4 = cadr(car(cdr(p1)));
              b2 = cadr(car(cdr(cdr(p1))));
              if (carp1 === symbol(MULTIPLY)) {
                push(a4);
                push(b2);
                multiply();
              } else if (isinnerordot(p1)) {
                push(b2);
                push(a4);
                inner();
              }
              push_integer(1);
              push_integer(2);
              originalexpanding = expanding;
              expanding = false;
              transpose();
              expanding = originalexpanding;
              p2 = pop();
              if (count(p2) < count(p1)) {
                p1 = p2;
              }
              if (DEBUG_SIMPLIFY) {
                return console.log("collecting a transpose " + p2);
              }
            }
          }
        };
        f4 = function() {
          if (isZeroAtomOrTensor(p1)) {
            return;
          }
          push(p1);
          rationalize();
          inverse();
          rationalize();
          inverse();
          rationalize();
          p2 = pop();
          if (count(p2) < count(p1)) {
            return p1 = p2;
          }
        };
        simplify_trig = function() {
          save();
          p1 = pop();
          f5();
          push(p1);
          return restore();
        };
        f5 = function() {
          if (Find(p1, symbol(SIN)) === 0 && Find(p1, symbol(COS)) === 0) {
            return;
          }
          p2 = p1;
          trigmode = 1;
          push(p2);
          Eval();
          p3 = pop();
          trigmode = 2;
          push(p2);
          Eval();
          p4 = pop();
          trigmode = 0;
          if (count(p4) < count(p3) || nterms(p4) < nterms(p3)) {
            p3 = p4;
          }
          if (count(p3) < count(p1) || nterms(p3) < nterms(p1)) {
            return p1 = p3;
          }
        };
        f9 = function() {
          var oldp1, oldp2;
          if (car(p1) !== symbol(ADD)) {
            return;
          }
          push_integer(0);
          p2 = cdr(p1);
          while (iscons(p2)) {
            push(car(p2));
            simplify();
            add();
            oldp1 = p1;
            oldp2 = p2;
            p1 = pop();
            simplify_rational_expressions();
            push(p1);
            p1 = oldp1;
            p2 = oldp2;
            p2 = cdr(p2);
          }
          p2 = pop();
          if (count(p2) < count(p1)) {
            return p1 = p2;
          }
        };
        simplify_rational_expressions = function() {
          var denom, num, polyVar, sasa, theGCD;
          push(p1);
          denominator();
          denom = pop();
          if (isone(denom)) {
            return;
          }
          push(p1);
          numerator();
          num = pop();
          if (isone(num)) {
            return;
          }
          if (!(polyVar = areunivarpolysfactoredorexpandedform(num, denom))) {
            return;
          }
          push(num);
          push(denom);
          gcd();
          push(polyVar);
          factor();
          theGCD = pop();
          if (isone(theGCD)) {
            return;
          }
          push(num);
          push(polyVar);
          factor();
          push(theGCD);
          inverse();
          multiply_noexpand();
          simplify();
          sasa = stack[tos - 1].toString();
          push(denom);
          push(polyVar);
          factor();
          push(theGCD);
          inverse();
          multiply_noexpand();
          simplify();
          sasa = stack[tos - 1].toString();
          divide();
          Condense();
          sasa = stack[tos - 1].toString();
          p2 = pop();
          if (count(p2) < count(p1)) {
            return p1 = p2;
          }
        };
        simplify_rectToClock = function() {
          if (Find(p1, symbol(SIN)) === 0 && Find(p1, symbol(COS)) === 0) {
            return;
          }
          push(p1);
          Eval();
          clockform();
          p2 = pop();
          if (DEBUG_SIMPLIFY) {
            console.log("before simplification clockform: " + p1 + " after: " + p2);
          }
          if (count(p2) < count(p1)) {
            return p1 = p2;
          }
        };
        simplify_polarRect = function() {
          push(p1);
          polarRectAMinusOneBase();
          Eval();
          p2 = pop();
          if (count(p2) < count(p1)) {
            return p1 = p2;
          }
        };
        polarRectAMinusOneBase = function() {
          var h5;
          save();
          p1 = pop();
          if (isimaginaryunit(p1)) {
            push(p1);
            restore();
            return;
          }
          if (equal(car(p1), symbol(POWER)) && isminusone(cadr(p1))) {
            push(one);
            negate();
            push(caddr(p1));
            polarRectAMinusOneBase();
            power();
            polar();
            rect();
          } else if (iscons(p1)) {
            h5 = tos;
            while (iscons(p1)) {
              push(car(p1));
              polarRectAMinusOneBase();
              p1 = cdr(p1);
            }
            list(tos - h5);
          } else {
            push(p1);
          }
          restore();
        };
        nterms = function(p11) {
          if (car(p11) !== symbol(ADD)) {
            return 1;
          } else {
            return length(p11) - 1;
          }
        };
        simplify_nested_radicals = function() {
          var prev_expanding, simplificationWithCondense, simplificationWithoutCondense, somethingSimplified;
          if (recursionLevelNestedRadicalsRemoval > 0) {
            if (DEBUG_SIMPLIFY) {
              console.log("denesting bailing out because of too much recursion");
            }
            return false;
          }
          push(p1);
          somethingSimplified = take_care_of_nested_radicals();
          simplificationWithoutCondense = stack[tos - 1];
          prev_expanding = expanding;
          expanding = 0;
          yycondense();
          expanding = prev_expanding;
          simplificationWithCondense = pop();
          if (countOccurrencesOfSymbol(symbol(POWER), simplificationWithoutCondense) < countOccurrencesOfSymbol(symbol(POWER), simplificationWithCondense)) {
            push(simplificationWithoutCondense);
          } else {
            push(simplificationWithCondense);
          }
          p1 = pop();
          return somethingSimplified;
        };
        take_care_of_nested_radicals = function() {
          var A2, B2, C7, SOLUTION, anyRadicalSimplificationWorked, base, checkSize, commonBases, commonInnerExponent, countingTerms, eachSolution, exponent, firstTerm, h5, i5, innerbase, innerexponent, l1, len, len1, len2, len3, lowercase_a, lowercase_b, m1, n1, numberOfTerms, o1, possibleNewExpression, possibleNewExpressionValue, possibleRationalSolutions, possibleSolutions, potentialPower, realOfpossibleRationalSolutions, ref2, secondTerm, secondTermFactor, termsThatAreNotPowers, whichRationalSolution;
          if (recursionLevelNestedRadicalsRemoval > 0) {
            if (DEBUG_SIMPLIFY) {
              console.log("denesting bailing out because of too much recursion");
            }
            return false;
          }
          save();
          p1 = pop();
          if (equal(car(p1), symbol(POWER))) {
            base = cadr(p1);
            exponent = caddr(p1);
            if (!isminusone(exponent) && equal(car(base), symbol(ADD)) && isfraction(exponent) && (equalq(exponent, 1, 3) || equalq(exponent, 1, 2))) {
              firstTerm = cadr(base);
              push(firstTerm);
              take_care_of_nested_radicals();
              pop();
              secondTerm = caddr(base);
              push(secondTerm);
              take_care_of_nested_radicals();
              pop();
              numberOfTerms = 0;
              countingTerms = base;
              while (cdr(countingTerms) !== symbol(NIL)) {
                numberOfTerms++;
                countingTerms = cdr(countingTerms);
              }
              if (numberOfTerms > 2) {
                push(p1);
                restore();
                return false;
              }
              commonInnerExponent = null;
              commonBases = [];
              termsThatAreNotPowers = [];
              if (car(secondTerm) === symbol(MULTIPLY)) {
                secondTermFactor = cdr(secondTerm);
                if (iscons(secondTermFactor)) {
                  while (iscons(secondTermFactor)) {
                    potentialPower = car(secondTermFactor);
                    if (car(potentialPower) === symbol(POWER)) {
                      innerbase = cadr(potentialPower);
                      innerexponent = caddr(potentialPower);
                      if (equalq(innerexponent, 1, 2)) {
                        if (commonInnerExponent == null) {
                          commonInnerExponent = innerexponent;
                          commonBases.push(innerbase);
                        } else {
                          if (equal(innerexponent, commonInnerExponent)) {
                            commonBases.push(innerbase);
                          } else {
                          }
                        }
                      }
                    } else {
                      termsThatAreNotPowers.push(potentialPower);
                    }
                    secondTermFactor = cdr(secondTermFactor);
                  }
                }
              } else if (car(secondTerm) === symbol(POWER)) {
                innerbase = cadr(secondTerm);
                innerexponent = caddr(secondTerm);
                if (commonInnerExponent == null && equalq(innerexponent, 1, 2)) {
                  commonInnerExponent = innerexponent;
                  commonBases.push(innerbase);
                }
              }
              if (commonBases.length === 0) {
                push(p1);
                restore();
                return false;
              }
              A2 = firstTerm;
              push_integer(1);
              for (l1 = 0, len = commonBases.length; l1 < len; l1++) {
                i5 = commonBases[l1];
                push(i5);
                multiply();
              }
              C7 = pop();
              push_integer(1);
              for (m1 = 0, len1 = termsThatAreNotPowers.length; m1 < len1; m1++) {
                i5 = termsThatAreNotPowers[m1];
                push(i5);
                multiply();
              }
              B2 = pop();
              if (equalq(exponent, 1, 3)) {
                push(A2);
                negate();
                push(C7);
                multiply();
                push(B2);
                divide();
                checkSize = pop();
                push(checkSize);
                real();
                yyfloat();
                if (Math.abs(pop().d) > Math.pow(2, 32)) {
                  push(p1);
                  restore();
                  return false;
                }
                push(checkSize);
                push_integer(3);
                push(C7);
                multiply();
                checkSize = pop();
                push(checkSize);
                real();
                yyfloat();
                if (Math.abs(pop().d) > Math.pow(2, 32)) {
                  pop();
                  push(p1);
                  restore();
                  return false;
                }
                push(checkSize);
                push(symbol(SECRETX));
                multiply();
                push_integer(-3);
                push(A2);
                multiply();
                push(B2);
                divide();
                checkSize = pop();
                push(checkSize);
                real();
                yyfloat();
                if (Math.abs(pop().d) > Math.pow(2, 32)) {
                  pop();
                  pop();
                  push(p1);
                  restore();
                  return false;
                }
                push(checkSize);
                push(symbol(SECRETX));
                push_integer(2);
                power();
                multiply();
                push_integer(1);
                push(symbol(SECRETX));
                push_integer(3);
                power();
                multiply();
                add();
                add();
                add();
              } else if (equalq(exponent, 1, 2)) {
                push(C7);
                checkSize = pop();
                push(checkSize);
                real();
                yyfloat();
                if (Math.abs(pop().d) > Math.pow(2, 32)) {
                  push(p1);
                  restore();
                  return false;
                }
                push(checkSize);
                push_integer(-2);
                push(A2);
                multiply();
                push(B2);
                divide();
                checkSize = pop();
                push(checkSize);
                real();
                yyfloat();
                if (Math.abs(pop().d) > Math.pow(2, 32)) {
                  pop();
                  push(p1);
                  restore();
                  return false;
                }
                push(checkSize);
                push(symbol(SECRETX));
                multiply();
                push_integer(1);
                push(symbol(SECRETX));
                push_integer(2);
                power();
                multiply();
                add();
                add();
              }
              push(symbol(SECRETX));
              recursionLevelNestedRadicalsRemoval++;
              roots();
              recursionLevelNestedRadicalsRemoval--;
              if (equal(stack[tos - 1], symbol(NIL))) {
                if (DEBUG_SIMPLIFY) {
                  console.log("roots bailed out because of too much recursion");
                }
                pop();
                push(p1);
                restore();
                return false;
              }
              possibleSolutions = [];
              ref2 = stack[tos - 1].tensor.elem;
              for (n1 = 0, len2 = ref2.length; n1 < len2; n1++) {
                eachSolution = ref2[n1];
                if (!Find(eachSolution, symbol(POWER))) {
                  possibleSolutions.push(eachSolution);
                }
              }
              pop();
              if (possibleSolutions.length === 0) {
                push(p1);
                restore();
                return false;
              }
              possibleRationalSolutions = [];
              realOfpossibleRationalSolutions = [];
              for (o1 = 0, len3 = possibleSolutions.length; o1 < len3; o1++) {
                i5 = possibleSolutions[o1];
                push(i5);
                real();
                yyfloat();
                possibleRationalSolutions.push(i5);
                realOfpossibleRationalSolutions.push(pop().d);
              }
              whichRationalSolution = realOfpossibleRationalSolutions.indexOf(Math.max.apply(Math, realOfpossibleRationalSolutions));
              SOLUTION = possibleRationalSolutions[whichRationalSolution];
              if (equalq(exponent, 1, 3)) {
                push(A2);
                push(SOLUTION);
                push_integer(3);
                power();
                push_integer(3);
                push(C7);
                multiply();
                push(SOLUTION);
                multiply();
                add();
                divide();
                push_rational(1, 3);
                power();
              } else if (equalq(exponent, 1, 2)) {
                push(A2);
                push(SOLUTION);
                push_integer(2);
                power();
                push(C7);
                add();
                divide();
                push_rational(1, 2);
                power();
              }
              lowercase_b = pop();
              if (lowercase_b == null) {
                push(p1);
                restore();
                return false;
              }
              push(lowercase_b);
              push(SOLUTION);
              multiply();
              if (equalq(exponent, 1, 3)) {
                lowercase_a = pop();
                push(lowercase_b);
                push(C7);
                push_rational(1, 2);
                power();
                multiply();
                push(lowercase_a);
                add();
                simplify();
              } else if (equalq(exponent, 1, 2)) {
                lowercase_a = pop();
                push(lowercase_b);
                push(C7);
                push_rational(1, 2);
                power();
                multiply();
                push(lowercase_a);
                add();
                simplify();
                possibleNewExpression = pop();
                push(possibleNewExpression);
                real();
                yyfloat();
                possibleNewExpressionValue = pop();
                if (!isnegativenumber(possibleNewExpressionValue)) {
                  push(possibleNewExpression);
                } else {
                  push(lowercase_b);
                  negate();
                  lowercase_b = pop();
                  push(lowercase_a);
                  negate();
                  lowercase_a = pop();
                  push(lowercase_b);
                  push(C7);
                  push_rational(1, 2);
                  power();
                  multiply();
                  push(lowercase_a);
                  add();
                  simplify();
                }
              }
              p1 = pop();
              push(p1);
              restore();
              return true;
            } else {
              push(p1);
              restore();
              return false;
            }
          } else if (iscons(p1)) {
            h5 = tos;
            anyRadicalSimplificationWorked = false;
            while (iscons(p1)) {
              push(car(p1));
              anyRadicalSimplificationWorked = anyRadicalSimplificationWorked || take_care_of_nested_radicals();
              p1 = cdr(p1);
            }
            list(tos - h5);
            restore();
            return anyRadicalSimplificationWorked;
          } else {
            push(p1);
            restore();
            return false;
          }
          throw new Error("control flow should never reach here");
        };
        Eval_sin = function() {
          push(cadr(p1));
          Eval();
          return sine();
        };
        sine = function() {
          save();
          p1 = pop();
          if (car(p1) === symbol(ADD)) {
            sine_of_angle_sum();
          } else {
            sine_of_angle();
          }
          return restore();
        };
        sine_of_angle_sum = function() {
          p2 = cdr(p1);
          while (iscons(p2)) {
            p4 = car(p2);
            if (isnpi(p4)) {
              push(p1);
              push(p4);
              subtract();
              p3 = pop();
              push(p3);
              sine();
              push(p4);
              cosine();
              multiply();
              push(p3);
              cosine();
              push(p4);
              sine();
              multiply();
              add();
              return;
            }
            p2 = cdr(p2);
          }
          return sine_of_angle();
        };
        sine_of_angle = function() {
          var d3, n9;
          if (car(p1) === symbol(ARCSIN)) {
            push(cadr(p1));
            return;
          }
          if (isdouble(p1)) {
            d3 = Math.sin(p1.d);
            if (Math.abs(d3) < 1e-10) {
              d3 = 0;
            }
            push_double(d3);
            return;
          }
          if (isnegative(p1)) {
            push(p1);
            negate();
            sine();
            negate();
            return;
          }
          if (car(p1) === symbol(ARCTAN)) {
            push(cadr(p1));
            push_integer(1);
            push(cadr(p1));
            push_integer(2);
            power();
            add();
            push_rational(-1, 2);
            power();
            multiply();
            return;
          }
          push(p1);
          push_integer(180);
          multiply();
          if (evaluatingAsFloats) {
            push_double(Math.PI);
          } else {
            push_symbol(PI);
          }
          divide();
          n9 = pop_integer();
          if (n9 < 0 || isNaN(n9)) {
            push(symbol(SIN));
            push(p1);
            list(2);
            return;
          }
          switch (n9 % 360) {
            case 0:
            case 180:
              return push_integer(0);
            case 30:
            case 150:
              return push_rational(1, 2);
            case 210:
            case 330:
              return push_rational(-1, 2);
            case 45:
            case 135:
              push_rational(1, 2);
              push_integer(2);
              push_rational(1, 2);
              power();
              return multiply();
            case 225:
            case 315:
              push_rational(-1, 2);
              push_integer(2);
              push_rational(1, 2);
              power();
              return multiply();
            case 60:
            case 120:
              push_rational(1, 2);
              push_integer(3);
              push_rational(1, 2);
              power();
              return multiply();
            case 240:
            case 300:
              push_rational(-1, 2);
              push_integer(3);
              push_rational(1, 2);
              power();
              return multiply();
            case 90:
              return push_integer(1);
            case 270:
              return push_integer(-1);
            default:
              push(symbol(SIN));
              push(p1);
              return list(2);
          }
        };
        Eval_sinh = function() {
          push(cadr(p1));
          Eval();
          return ysinh();
        };
        ysinh = function() {
          save();
          yysinh();
          return restore();
        };
        yysinh = function() {
          var d3;
          d3 = 0;
          p1 = pop();
          if (car(p1) === symbol(ARCSINH)) {
            push(cadr(p1));
            return;
          }
          if (isdouble(p1)) {
            d3 = Math.sinh(p1.d);
            if (Math.abs(d3) < 1e-10) {
              d3 = 0;
            }
            push_double(d3);
            return;
          }
          if (isZeroAtomOrTensor(p1)) {
            push(zero);
            return;
          }
          push_symbol(SINH);
          push(p1);
          return list(2);
        };
        subst = function() {
          var expr, i5, l1, m1, newExpr, newTensor, oldExpr, ref2, ref3;
          save();
          newExpr = pop();
          oldExpr = pop();
          if (oldExpr === symbol(NIL) || newExpr === symbol(NIL)) {
            restore();
            return;
          }
          expr = pop();
          if (istensor(expr)) {
            newTensor = alloc_tensor(expr.tensor.nelem);
            newTensor.tensor.ndim = expr.tensor.ndim;
            for (i5 = l1 = 0, ref2 = expr.tensor.ndim; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
              newTensor.tensor.dim[i5] = expr.tensor.dim[i5];
            }
            for (i5 = m1 = 0, ref3 = expr.tensor.nelem; 0 <= ref3 ? m1 < ref3 : m1 > ref3; i5 = 0 <= ref3 ? ++m1 : --m1) {
              push(expr.tensor.elem[i5]);
              push(oldExpr);
              push(newExpr);
              subst();
              newTensor.tensor.elem[i5] = pop();
              check_tensor_dimensions(newTensor);
            }
            push(newTensor);
          } else if (equal(expr, oldExpr)) {
            push(newExpr);
          } else if (iscons(expr)) {
            push(car(expr));
            push(oldExpr);
            push(newExpr);
            subst();
            push(cdr(expr));
            push(oldExpr);
            push(newExpr);
            subst();
            cons();
          } else {
            push(expr);
          }
          return restore();
        };
        Eval_sum = function() {
          var body, i5, indexVariable, j2, k2, l1, ref2, ref3;
          i5 = 0;
          j2 = 0;
          k2 = 0;
          body = cadr(p1);
          indexVariable = caddr(p1);
          if (!issymbol(indexVariable)) {
            stop("sum: 2nd arg?");
          }
          push(cadddr(p1));
          Eval();
          j2 = pop_integer();
          if (isNaN(j2)) {
            push(p1);
            return;
          }
          push(caddddr(p1));
          Eval();
          k2 = pop_integer();
          if (isNaN(k2)) {
            push(p1);
            return;
          }
          p4 = get_binding(indexVariable);
          push_integer(0);
          for (i5 = l1 = ref2 = j2, ref3 = k2; ref2 <= ref3 ? l1 <= ref3 : l1 >= ref3; i5 = ref2 <= ref3 ? ++l1 : --l1) {
            push_integer(i5);
            p5 = pop();
            set_binding(indexVariable, p5);
            push(body);
            Eval();
            add();
          }
          return set_binding(indexVariable, p4);
        };
        Eval_tan = function() {
          push(cadr(p1));
          Eval();
          return tangent();
        };
        tangent = function() {
          save();
          yytangent();
          return restore();
        };
        yytangent = function() {
          var d3, n9;
          n9 = 0;
          d3 = 0;
          p1 = pop();
          if (car(p1) === symbol(ARCTAN)) {
            push(cadr(p1));
            return;
          }
          if (isdouble(p1)) {
            d3 = Math.tan(p1.d);
            if (Math.abs(d3) < 1e-10) {
              d3 = 0;
            }
            push_double(d3);
            return;
          }
          if (isnegative(p1)) {
            push(p1);
            negate();
            tangent();
            negate();
            return;
          }
          push(p1);
          push_integer(180);
          multiply();
          if (evaluatingAsFloats) {
            push_double(Math.PI);
          } else {
            push_symbol(PI);
          }
          divide();
          n9 = pop_integer();
          if (n9 < 0 || isNaN(n9)) {
            push(symbol(TAN));
            push(p1);
            list(2);
            return;
          }
          switch (n9 % 360) {
            case 0:
            case 180:
              return push_integer(0);
            case 30:
            case 210:
              push_rational(1, 3);
              push_integer(3);
              push_rational(1, 2);
              power();
              return multiply();
            case 150:
            case 330:
              push_rational(-1, 3);
              push_integer(3);
              push_rational(1, 2);
              power();
              return multiply();
            case 45:
            case 225:
              return push_integer(1);
            case 135:
            case 315:
              return push_integer(-1);
            case 60:
            case 240:
              push_integer(3);
              push_rational(1, 2);
              return power();
            case 120:
            case 300:
              push_integer(3);
              push_rational(1, 2);
              power();
              return negate();
            default:
              push(symbol(TAN));
              push(p1);
              return list(2);
          }
        };
        Eval_tanh = function() {
          var d3;
          d3 = 0;
          push(cadr(p1));
          Eval();
          p1 = pop();
          if (car(p1) === symbol(ARCTANH)) {
            push(cadr(p1));
            return;
          }
          if (isdouble(p1)) {
            d3 = Math.tanh(p1.d);
            if (Math.abs(d3) < 1e-10) {
              d3 = 0;
            }
            push_double(d3);
            return;
          }
          if (isZeroAtomOrTensor(p1)) {
            push(zero);
            return;
          }
          push_symbol(TANH);
          push(p1);
          return list(2);
        };
        Eval_taylor = function() {
          p1 = cdr(p1);
          push(car(p1));
          Eval();
          p1 = cdr(p1);
          push(car(p1));
          Eval();
          p2 = pop();
          if (p2 === symbol(NIL)) {
            guess();
          } else {
            push(p2);
          }
          p1 = cdr(p1);
          push(car(p1));
          Eval();
          p2 = pop();
          if (p2 === symbol(NIL)) {
            push_integer(24);
          } else {
            push(p2);
          }
          p1 = cdr(p1);
          push(car(p1));
          Eval();
          p2 = pop();
          if (p2 === symbol(NIL)) {
            push_integer(0);
          } else {
            push(p2);
          }
          return taylor();
        };
        taylor = function() {
          var i5, k2, l1, ref2;
          i5 = 0;
          k2 = 0;
          save();
          p4 = pop();
          p3 = pop();
          p2 = pop();
          p1 = pop();
          push(p3);
          k2 = pop_integer();
          if (isNaN(k2)) {
            push_symbol(TAYLOR);
            push(p1);
            push(p2);
            push(p3);
            push(p4);
            list(5);
            restore();
            return;
          }
          push(p1);
          push(p2);
          push(p4);
          subst();
          Eval();
          push_integer(1);
          p5 = pop();
          for (i5 = l1 = 1, ref2 = k2; 1 <= ref2 ? l1 <= ref2 : l1 >= ref2; i5 = 1 <= ref2 ? ++l1 : --l1) {
            push(p1);
            push(p2);
            derivative();
            p1 = pop();
            if (isZeroAtomOrTensor(p1)) {
              break;
            }
            push(p5);
            push(p2);
            push(p4);
            subtract();
            multiply();
            p5 = pop();
            push(p1);
            push(p2);
            push(p4);
            subst();
            Eval();
            push(p5);
            multiply();
            push_integer(i5);
            factorial();
            divide();
            add();
          }
          return restore();
        };
        Eval_tensor = function() {
          var a4, b2, i5, l1, m1, ndim, nelem, ref2, ref3;
          i5 = 0;
          ndim = 0;
          nelem = 0;
          check_tensor_dimensions(p1);
          nelem = p1.tensor.nelem;
          ndim = p1.tensor.ndim;
          p2 = alloc_tensor(nelem);
          p2.tensor.ndim = ndim;
          for (i5 = l1 = 0, ref2 = ndim; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
            p2.tensor.dim[i5] = p1.tensor.dim[i5];
          }
          a4 = p1.tensor.elem;
          b2 = p2.tensor.elem;
          check_tensor_dimensions(p2);
          for (i5 = m1 = 0, ref3 = nelem; 0 <= ref3 ? m1 < ref3 : m1 > ref3; i5 = 0 <= ref3 ? ++m1 : --m1) {
            push(a4[i5]);
            Eval();
            b2[i5] = pop();
          }
          check_tensor_dimensions(p1);
          check_tensor_dimensions(p2);
          push(p2);
          return promote_tensor();
        };
        tensor_plus_tensor = function() {
          var a4, b2, c6, i5, l1, m1, n1, ndim, nelem, ref2, ref3, ref4;
          i5 = 0;
          ndim = 0;
          nelem = 0;
          save();
          p2 = pop();
          p1 = pop();
          ndim = p1.tensor.ndim;
          if (ndim !== p2.tensor.ndim) {
            push(symbol(NIL));
            restore();
            return;
          }
          for (i5 = l1 = 0, ref2 = ndim; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
            if (p1.tensor.dim[i5] !== p2.tensor.dim[i5]) {
              push(symbol(NIL));
              restore();
              return;
            }
          }
          nelem = p1.tensor.nelem;
          p3 = alloc_tensor(nelem);
          p3.tensor.ndim = ndim;
          for (i5 = m1 = 0, ref3 = ndim; 0 <= ref3 ? m1 < ref3 : m1 > ref3; i5 = 0 <= ref3 ? ++m1 : --m1) {
            p3.tensor.dim[i5] = p1.tensor.dim[i5];
          }
          a4 = p1.tensor.elem;
          b2 = p2.tensor.elem;
          c6 = p3.tensor.elem;
          for (i5 = n1 = 0, ref4 = nelem; 0 <= ref4 ? n1 < ref4 : n1 > ref4; i5 = 0 <= ref4 ? ++n1 : --n1) {
            push(a4[i5]);
            push(b2[i5]);
            add();
            c6[i5] = pop();
          }
          push(p3);
          return restore();
        };
        tensor_times_scalar = function() {
          var a4, b2, i5, l1, m1, ndim, nelem, ref2, ref3;
          i5 = 0;
          ndim = 0;
          nelem = 0;
          save();
          p2 = pop();
          p1 = pop();
          ndim = p1.tensor.ndim;
          nelem = p1.tensor.nelem;
          p3 = alloc_tensor(nelem);
          p3.tensor.ndim = ndim;
          for (i5 = l1 = 0, ref2 = ndim; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
            p3.tensor.dim[i5] = p1.tensor.dim[i5];
          }
          a4 = p1.tensor.elem;
          b2 = p3.tensor.elem;
          for (i5 = m1 = 0, ref3 = nelem; 0 <= ref3 ? m1 < ref3 : m1 > ref3; i5 = 0 <= ref3 ? ++m1 : --m1) {
            push(a4[i5]);
            push(p2);
            multiply();
            b2[i5] = pop();
          }
          push(p3);
          return restore();
        };
        scalar_times_tensor = function() {
          var a4, b2, i5, l1, m1, ndim, nelem, ref2, ref3;
          i5 = 0;
          ndim = 0;
          nelem = 0;
          save();
          p2 = pop();
          p1 = pop();
          ndim = p2.tensor.ndim;
          nelem = p2.tensor.nelem;
          p3 = alloc_tensor(nelem);
          p3.tensor.ndim = ndim;
          for (i5 = l1 = 0, ref2 = ndim; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
            p3.tensor.dim[i5] = p2.tensor.dim[i5];
          }
          a4 = p2.tensor.elem;
          b2 = p3.tensor.elem;
          for (i5 = m1 = 0, ref3 = nelem; 0 <= ref3 ? m1 < ref3 : m1 > ref3; i5 = 0 <= ref3 ? ++m1 : --m1) {
            push(p1);
            push(a4[i5]);
            multiply();
            b2[i5] = pop();
          }
          push(p3);
          return restore();
        };
        check_tensor_dimensions = function(p11) {
          if (p11.tensor.nelem !== p11.tensor.elem.length) {
            console.log("something wrong in tensor dimensions");
            debugger;
          }
        };
        is_square_matrix = function(p11) {
          if (istensor(p11) && p11.tensor.ndim === 2 && p11.tensor.dim[0] === p11.tensor.dim[1]) {
            return 1;
          } else {
            return 0;
          }
        };
        d_tensor_tensor = function() {
          var a4, b2, c6, i5, j2, l1, m1, n1, ndim, nelem, ref2, ref3, ref4;
          i5 = 0;
          j2 = 0;
          ndim = 0;
          nelem = 0;
          ndim = p1.tensor.ndim;
          nelem = p1.tensor.nelem;
          if (ndim + 1 >= MAXDIM) {
            push_symbol(DERIVATIVE);
            push(p1);
            push(p2);
            list(3);
            return;
          }
          p3 = alloc_tensor(nelem * p2.tensor.nelem);
          p3.tensor.ndim = ndim + 1;
          for (i5 = l1 = 0, ref2 = ndim; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
            p3.tensor.dim[i5] = p1.tensor.dim[i5];
          }
          p3.tensor.dim[ndim] = p2.tensor.dim[0];
          a4 = p1.tensor.elem;
          b2 = p2.tensor.elem;
          c6 = p3.tensor.elem;
          for (i5 = m1 = 0, ref3 = nelem; 0 <= ref3 ? m1 < ref3 : m1 > ref3; i5 = 0 <= ref3 ? ++m1 : --m1) {
            for (j2 = n1 = 0, ref4 = p2.tensor.nelem; 0 <= ref4 ? n1 < ref4 : n1 > ref4; j2 = 0 <= ref4 ? ++n1 : --n1) {
              push(a4[i5]);
              push(b2[j2]);
              derivative();
              c6[i5 * p2.tensor.nelem + j2] = pop();
            }
          }
          return push(p3);
        };
        d_scalar_tensor = function() {
          var a4, b2, i5, l1, ref2;
          p3 = alloc_tensor(p2.tensor.nelem);
          p3.tensor.ndim = 1;
          p3.tensor.dim[0] = p2.tensor.dim[0];
          a4 = p2.tensor.elem;
          b2 = p3.tensor.elem;
          for (i5 = l1 = 0, ref2 = p2.tensor.nelem; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
            push(p1);
            push(a4[i5]);
            derivative();
            b2[i5] = pop();
          }
          return push(p3);
        };
        d_tensor_scalar = function() {
          var a4, b2, i5, l1, m1, ref2, ref3;
          i5 = 0;
          p3 = alloc_tensor(p1.tensor.nelem);
          p3.tensor.ndim = p1.tensor.ndim;
          for (i5 = l1 = 0, ref2 = p1.tensor.ndim; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
            p3.tensor.dim[i5] = p1.tensor.dim[i5];
          }
          a4 = p1.tensor.elem;
          b2 = p3.tensor.elem;
          for (i5 = m1 = 0, ref3 = p1.tensor.nelem; 0 <= ref3 ? m1 < ref3 : m1 > ref3; i5 = 0 <= ref3 ? ++m1 : --m1) {
            push(a4[i5]);
            push(p2);
            derivative();
            b2[i5] = pop();
          }
          return push(p3);
        };
        compare_tensors = function(p12, p22) {
          var i5, l1, m1, ref2, ref3;
          i5 = 0;
          if (p12.tensor.ndim < p22.tensor.ndim) {
            return -1;
          }
          if (p12.tensor.ndim > p22.tensor.ndim) {
            return 1;
          }
          for (i5 = l1 = 0, ref2 = p12.tensor.ndim; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
            if (p12.tensor.dim[i5] < p22.tensor.dim[i5]) {
              return -1;
            }
            if (p12.tensor.dim[i5] > p22.tensor.dim[i5]) {
              return 1;
            }
          }
          for (i5 = m1 = 0, ref3 = p12.tensor.nelem; 0 <= ref3 ? m1 < ref3 : m1 > ref3; i5 = 0 <= ref3 ? ++m1 : --m1) {
            if (equal(p12.tensor.elem[i5], p22.tensor.elem[i5])) {
              continue;
            }
            if (lessp(p12.tensor.elem[i5], p22.tensor.elem[i5])) {
              return -1;
            } else {
              return 1;
            }
          }
          return 0;
        };
        power_tensor = function() {
          var i5, k2, l1, m1, n9, ref2, ref3, results;
          i5 = 0;
          k2 = 0;
          n9 = 0;
          k2 = p1.tensor.ndim - 1;
          if (p1.tensor.dim[0] !== p1.tensor.dim[k2]) {
            push_symbol(POWER);
            push(p1);
            push(p2);
            list(3);
            return;
          }
          push(p2);
          n9 = pop_integer();
          if (isNaN(n9)) {
            push_symbol(POWER);
            push(p1);
            push(p2);
            list(3);
            return;
          }
          if (n9 === 0) {
            if (p1.tensor.ndim !== 2) {
              stop("power(tensor,0) with tensor rank not equal to 2");
            }
            n9 = p1.tensor.dim[0];
            p1 = alloc_tensor(n9 * n9);
            p1.tensor.ndim = 2;
            p1.tensor.dim[0] = n9;
            p1.tensor.dim[1] = n9;
            for (i5 = l1 = 0, ref2 = n9; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
              p1.tensor.elem[n9 * i5 + i5] = one;
            }
            check_tensor_dimensions(p1);
            push(p1);
            return;
          }
          if (n9 < 0) {
            n9 = -n9;
            push(p1);
            inv();
            p1 = pop();
          }
          push(p1);
          results = [];
          for (i5 = m1 = 1, ref3 = n9; 1 <= ref3 ? m1 < ref3 : m1 > ref3; i5 = 1 <= ref3 ? ++m1 : --m1) {
            push(p1);
            inner();
            if (isZeroAtomOrTensor(stack[tos - 1])) {
              break;
            } else {
              results.push(void 0);
            }
          }
          return results;
        };
        copy_tensor = function() {
          var i5, l1, m1, ref2, ref3;
          i5 = 0;
          save();
          p1 = pop();
          p2 = alloc_tensor(p1.tensor.nelem);
          p2.tensor.ndim = p1.tensor.ndim;
          for (i5 = l1 = 0, ref2 = p1.tensor.ndim; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
            p2.tensor.dim[i5] = p1.tensor.dim[i5];
          }
          for (i5 = m1 = 0, ref3 = p1.tensor.nelem; 0 <= ref3 ? m1 < ref3 : m1 > ref3; i5 = 0 <= ref3 ? ++m1 : --m1) {
            p2.tensor.elem[i5] = p1.tensor.elem[i5];
          }
          check_tensor_dimensions(p1);
          check_tensor_dimensions(p2);
          push(p2);
          return restore();
        };
        promote_tensor = function() {
          var i5, j2, k2, l1, m1, n1, ndim, nelem, o1, q1, ref2, ref3, ref4, ref5, ref6;
          i5 = 0;
          j2 = 0;
          k2 = 0;
          nelem = 0;
          ndim = 0;
          save();
          p1 = pop();
          if (!istensor(p1)) {
            push(p1);
            restore();
            return;
          }
          p2 = p1.tensor.elem[0];
          for (i5 = l1 = 1, ref2 = p1.tensor.nelem; 1 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 1 <= ref2 ? ++l1 : --l1) {
            if (!compatible(p2, p1.tensor.elem[i5])) {
              stop("Cannot promote tensor due to inconsistent tensor components.");
            }
          }
          if (!istensor(p2)) {
            push(p1);
            restore();
            return;
          }
          ndim = p1.tensor.ndim + p2.tensor.ndim;
          if (ndim > MAXDIM) {
            stop("tensor rank > " + MAXDIM);
          }
          nelem = p1.tensor.nelem * p2.tensor.nelem;
          p3 = alloc_tensor(nelem);
          p3.tensor.ndim = ndim;
          for (i5 = m1 = 0, ref3 = p1.tensor.ndim; 0 <= ref3 ? m1 < ref3 : m1 > ref3; i5 = 0 <= ref3 ? ++m1 : --m1) {
            p3.tensor.dim[i5] = p1.tensor.dim[i5];
          }
          for (j2 = n1 = 0, ref4 = p2.tensor.ndim; 0 <= ref4 ? n1 < ref4 : n1 > ref4; j2 = 0 <= ref4 ? ++n1 : --n1) {
            p3.tensor.dim[i5 + j2] = p2.tensor.dim[j2];
          }
          k2 = 0;
          for (i5 = o1 = 0, ref5 = p1.tensor.nelem; 0 <= ref5 ? o1 < ref5 : o1 > ref5; i5 = 0 <= ref5 ? ++o1 : --o1) {
            p2 = p1.tensor.elem[i5];
            for (j2 = q1 = 0, ref6 = p2.tensor.nelem; 0 <= ref6 ? q1 < ref6 : q1 > ref6; j2 = 0 <= ref6 ? ++q1 : --q1) {
              p3.tensor.elem[k2++] = p2.tensor.elem[j2];
            }
          }
          check_tensor_dimensions(p2);
          check_tensor_dimensions(p3);
          push(p3);
          return restore();
        };
        compatible = function(p11, q) {
          var i5, l1, ref2;
          if (!istensor(p11) && !istensor(q)) {
            return 1;
          }
          if (!istensor(p11) || !istensor(q)) {
            return 0;
          }
          if (p11.tensor.ndim !== q.tensor.ndim) {
            return 0;
          }
          for (i5 = l1 = 0, ref2 = p11.tensor.ndim; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
            if (p11.tensor.dim[i5] !== q.tensor.dim[i5]) {
              return 0;
            }
          }
          return 1;
        };
        Eval_test = function() {
          var checkResult, orig;
          orig = p1;
          p1 = cdr(p1);
          while (iscons(p1)) {
            if (cdr(p1) === symbol(NIL)) {
              push(car(p1));
              Eval();
              return;
            }
            checkResult = isZeroLikeOrNonZeroLikeOrUndetermined(car(p1));
            if (checkResult == null) {
              push(orig);
              return;
            } else if (checkResult) {
              push(cadr(p1));
              Eval();
              return;
            } else {
              p1 = cddr(p1);
            }
          }
          return push_integer(0);
        };
        Eval_testeq = function() {
          var checkResult, orig, subtractionResult;
          orig = p1;
          push(cadr(p1));
          Eval();
          push(caddr(p1));
          Eval();
          subtract();
          subtractionResult = pop();
          checkResult = isZeroLikeOrNonZeroLikeOrUndetermined(subtractionResult);
          if (checkResult) {
            push_integer(0);
            return;
          } else if (checkResult != null && !checkResult) {
            push_integer(1);
            return;
          }
          push(cadr(p1));
          Eval();
          simplify();
          push(caddr(p1));
          Eval();
          simplify();
          subtract();
          subtractionResult = pop();
          checkResult = isZeroLikeOrNonZeroLikeOrUndetermined(subtractionResult);
          if (checkResult) {
            push_integer(0);
            return;
          } else if (checkResult != null && !checkResult) {
            push_integer(1);
            return;
          }
          return push(orig);
        };
        Eval_testge = function() {
          var comparison, orig;
          orig = p1;
          comparison = cmp_args();
          if (comparison == null) {
            push(orig);
            return;
          }
          if (comparison >= 0) {
            return push_integer(1);
          } else {
            return push_integer(0);
          }
        };
        Eval_testgt = function() {
          var comparison, orig;
          orig = p1;
          comparison = cmp_args();
          if (comparison == null) {
            push(orig);
            return;
          }
          if (comparison > 0) {
            return push_integer(1);
          } else {
            return push_integer(0);
          }
        };
        Eval_testle = function() {
          var comparison, orig;
          orig = p1;
          comparison = cmp_args();
          if (comparison == null) {
            push(orig);
            return;
          }
          if (comparison <= 0) {
            return push_integer(1);
          } else {
            return push_integer(0);
          }
        };
        Eval_testlt = function() {
          var comparison, orig;
          orig = p1;
          comparison = cmp_args();
          if (comparison == null) {
            push(orig);
            return;
          }
          if (comparison < 0) {
            return push_integer(1);
          } else {
            return push_integer(0);
          }
        };
        Eval_not = function() {
          var checkResult, wholeAndExpression;
          wholeAndExpression = p1;
          checkResult = isZeroLikeOrNonZeroLikeOrUndetermined(cadr(p1));
          if (checkResult == null) {
            return push(wholeAndExpression);
          } else if (checkResult) {
            return push_integer(0);
          } else {
            return push_integer(1);
          }
        };
        Eval_and = function() {
          var andPredicates, checkResult, somePredicateUnknown, wholeAndExpression;
          wholeAndExpression = p1;
          andPredicates = cdr(wholeAndExpression);
          somePredicateUnknown = false;
          while (iscons(andPredicates)) {
            checkResult = isZeroLikeOrNonZeroLikeOrUndetermined(car(andPredicates));
            if (checkResult == null) {
              somePredicateUnknown = true;
              andPredicates = cdr(andPredicates);
            } else if (checkResult) {
              andPredicates = cdr(andPredicates);
            } else if (!checkResult) {
              push_integer(0);
              return;
            }
          }
          if (somePredicateUnknown) {
            return push(wholeAndExpression);
          } else {
            return push_integer(1);
          }
        };
        Eval_or = function() {
          var checkResult, orPredicates, somePredicateUnknown, wholeOrExpression;
          wholeOrExpression = p1;
          orPredicates = cdr(wholeOrExpression);
          somePredicateUnknown = false;
          while (iscons(orPredicates)) {
            checkResult = isZeroLikeOrNonZeroLikeOrUndetermined(car(orPredicates));
            if (checkResult == null) {
              somePredicateUnknown = true;
              orPredicates = cdr(orPredicates);
            } else if (checkResult) {
              push_integer(1);
              return;
            } else if (!checkResult) {
              orPredicates = cdr(orPredicates);
            }
          }
          if (somePredicateUnknown) {
            return push(wholeOrExpression);
          } else {
            return push_integer(0);
          }
        };
        cmp_args = function() {
          var t5;
          t5 = 0;
          push(cadr(p1));
          Eval();
          simplify();
          push(caddr(p1));
          Eval();
          simplify();
          subtract();
          p1 = pop();
          if (p1.k !== NUM && p1.k !== DOUBLE) {
            push(p1);
            yyfloat();
            Eval();
            p1 = pop();
          }
          if (isZeroAtomOrTensor(p1)) {
            return 0;
          }
          switch (p1.k) {
            case NUM:
              if (MSIGN(p1.q.a) === -1) {
                t5 = -1;
              } else {
                t5 = 1;
              }
              break;
            case DOUBLE:
              if (p1.d < 0) {
                t5 = -1;
              } else {
                t5 = 1;
              }
              break;
            default:
              t5 = null;
          }
          return t5;
        };
        transform = function(s7, generalTransform) {
          var bookmarkTosToPrintDecomps, eachTransformEntry, i5, l1, len, len1, m1, n1, numberOfDecomps, ref2, restTerm, secondTerm, success, theTransform, transform_h, transformationSuccessful, transformedTerms;
          transform_h = 0;
          save();
          p1 = null;
          p4 = pop();
          p3 = pop();
          if (DEBUG) {
            console.log("         !!!!!!!!!   transform on: " + p3);
          }
          saveMetaBindings();
          set_binding(symbol(METAX), p4);
          transform_h = tos;
          push_integer(1);
          push(p3);
          push(p4);
          polyform();
          push(p4);
          bookmarkTosToPrintDecomps = tos - 2;
          decomp(generalTransform);
          numberOfDecomps = tos - bookmarkTosToPrintDecomps;
          if (DEBUG) {
            console.log("  " + numberOfDecomps + " decomposed elements ====== ");
            for (i5 = l1 = 0, ref2 = numberOfDecomps; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
              console.log("  decomposition element " + i5 + ": " + stack[tos - 1 - i5]);
            }
          }
          transformationSuccessful = false;
          if (generalTransform) {
            if (!isNumericAtom(p3)) {
              theTransform = s7;
              if (DEBUG) {
                console.log("applying transform: " + theTransform);
              }
              if (DEBUG) {
                console.log("scanning table entry " + theTransform);
              }
              push(theTransform);
              push(symbol(SYMBOL_A_UNDERSCORE));
              push(symbol(METAA));
              subst();
              push(symbol(SYMBOL_B_UNDERSCORE));
              push(symbol(METAB));
              subst();
              push(symbol(SYMBOL_X_UNDERSCORE));
              push(symbol(METAX));
              subst();
              p1 = pop();
              p5 = car(p1);
              if (DEBUG) {
                console.log("template expression: " + p5);
              }
              p6 = cadr(p1);
              p7 = cddr(p1);
              if (f_equals_a(transform_h, generalTransform)) {
                transformationSuccessful = true;
              } else {
                if (DEBUG) {
                  console.log("p3 at this point: " + p3);
                }
                transformedTerms = [];
                if (DEBUG) {
                  console.log("car(p3): " + car(p3));
                }
                restTerm = p3;
                if (iscons(restTerm)) {
                  transformedTerms.push(car(p3));
                  restTerm = cdr(p3);
                }
                while (iscons(restTerm)) {
                  secondTerm = car(restTerm);
                  restTerm = cdr(restTerm);
                  if (DEBUG) {
                    console.log("tos before recursive transform: " + tos);
                  }
                  push(secondTerm);
                  push_symbol(NIL);
                  if (DEBUG) {
                    console.log("testing: " + secondTerm);
                  }
                  if (DEBUG) {
                    console.log("about to try to simplify other term: " + secondTerm);
                  }
                  success = transform(s7, generalTransform);
                  transformationSuccessful = transformationSuccessful || success;
                  transformedTerms.push(pop());
                  if (DEBUG) {
                    console.log("tried to simplify other term: " + secondTerm + " ...successful?: " + success + " ...transformed: " + transformedTerms[transformedTerms.length - 1]);
                  }
                }
                if (transformedTerms.length !== 0) {
                  for (m1 = 0, len = transformedTerms.length; m1 < len; m1++) {
                    i5 = transformedTerms[m1];
                    push(i5);
                  }
                  list(transformedTerms.length);
                  p6 = pop();
                }
              }
            }
          } else {
            for (n1 = 0, len1 = s7.length; n1 < len1; n1++) {
              eachTransformEntry = s7[n1];
              if (DEBUG) {
                console.log("scanning table entry " + eachTransformEntry);
                if ((eachTransformEntry + "").indexOf("f(sqrt(a+b*x),2/3*1/b*sqrt((a+b*x)^3))") !== -1) {
                  debugger;
                }
              }
              if (eachTransformEntry) {
                scan_meta(eachTransformEntry);
                p1 = pop();
                p5 = cadr(p1);
                p6 = caddr(p1);
                p7 = cdddr(p1);
                if (f_equals_a(transform_h, generalTransform)) {
                  transformationSuccessful = true;
                  break;
                }
              }
            }
          }
          moveTos(transform_h);
          if (transformationSuccessful) {
            push(p6);
            Eval();
            p1 = pop();
            transformationSuccessful = true;
          } else {
            if (generalTransform) {
              p1 = p3;
            } else {
              p1 = symbol(NIL);
            }
          }
          restoreMetaBindings();
          push(p1);
          restore();
          return transformationSuccessful;
        };
        saveMetaBindings = function() {
          push(get_binding(symbol(METAA)));
          push(get_binding(symbol(METAB)));
          return push(get_binding(symbol(METAX)));
        };
        restoreMetaBindings = function() {
          set_binding(symbol(METAX), pop());
          set_binding(symbol(METAB), pop());
          return set_binding(symbol(METAA), pop());
        };
        f_equals_a = function(h5, generalTransform) {
          var fea_i, fea_j, l1, m1, originalexpanding, ref2, ref3, ref4, ref5;
          fea_i = 0;
          fea_j = 0;
          for (fea_i = l1 = ref2 = h5, ref3 = tos; ref2 <= ref3 ? l1 < ref3 : l1 > ref3; fea_i = ref2 <= ref3 ? ++l1 : --l1) {
            set_binding(symbol(METAA), stack[fea_i]);
            if (DEBUG) {
              console.log("  binding METAA to " + get_binding(symbol(METAA)));
            }
            for (fea_j = m1 = ref4 = h5, ref5 = tos; ref4 <= ref5 ? m1 < ref5 : m1 > ref5; fea_j = ref4 <= ref5 ? ++m1 : --m1) {
              set_binding(symbol(METAB), stack[fea_j]);
              if (DEBUG) {
                console.log("  binding METAB to " + get_binding(symbol(METAB)));
              }
              p1 = p7;
              while (iscons(p1)) {
                push(car(p1));
                Eval();
                p2 = pop();
                if (isZeroAtomOrTensor(p2)) {
                  break;
                }
                p1 = cdr(p1);
              }
              if (iscons(p1)) {
                continue;
              }
              push(p3);
              if (DEBUG) {
                console.log("about to evaluate template expression: " + p5 + " binding METAA to " + get_binding(symbol(METAA)) + " and binding METAB to " + get_binding(symbol(METAB)) + " and binding METAX to " + get_binding(symbol(METAX)));
              }
              push(p5);
              if (generalTransform) {
                originalexpanding = expanding;
                expanding = false;
              }
              Eval();
              if (generalTransform) {
                expanding = originalexpanding;
              }
              if (DEBUG) {
                console.log("  comparing " + stack[tos - 1] + " to: " + stack[tos - 2]);
              }
              subtract();
              p1 = pop();
              if (isZeroAtomOrTensor(p1)) {
                if (DEBUG) {
                  console.log("binding METAA to " + get_binding(symbol(METAA)));
                  console.log("binding METAB to " + get_binding(symbol(METAB)));
                  console.log("binding METAX to " + get_binding(symbol(METAX)));
                  console.log("comparing " + p3 + " to: " + p5);
                }
                return 1;
              }
            }
          }
          return 0;
        };
        Eval_transpose = function() {
          push(cadr(p1));
          Eval();
          if (cddr(p1) === symbol(NIL)) {
            push_integer(1);
            push_integer(2);
          } else {
            push(caddr(p1));
            Eval();
            push(cadddr(p1));
            Eval();
          }
          return transpose();
        };
        transpose = function() {
          var a4, accumulator, ai, an, b2, eachEntry, i5, innerTranspSwitch1, innerTranspSwitch2, j2, k2, l8, l1, m3, m1, n1, ndim, nelem, o1, q1, r1, ref2, ref3, ref4, ref5, ref6, ref7, ref8, s1, t5;
          i5 = 0;
          j2 = 0;
          k2 = 0;
          l8 = 0;
          m3 = 0;
          ndim = 0;
          nelem = 0;
          t5 = 0;
          ai = [];
          an = [];
          for (i5 = l1 = 0, ref2 = MAXDIM; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
            ai[i5] = 0;
            an[i5] = 0;
          }
          save();
          p3 = pop();
          p2 = pop();
          p1 = pop();
          if (isNumericAtom(p1)) {
            push(p1);
            restore();
            return;
          }
          if (isplusone(p2) && isplustwo(p3) || isplusone(p3) && isplustwo(p2)) {
            if (isidentitymatrix(p1)) {
              push(p1);
              restore();
              return;
            }
          }
          if (istranspose(p1)) {
            innerTranspSwitch1 = car(cdr(cdr(p1)));
            innerTranspSwitch2 = car(cdr(cdr(cdr(p1))));
            if (equal(innerTranspSwitch1, p3) && equal(innerTranspSwitch2, p2) || equal(innerTranspSwitch2, p3) && equal(innerTranspSwitch1, p2) || equal(innerTranspSwitch1, symbol(NIL)) && equal(innerTranspSwitch2, symbol(NIL)) && (isplusone(p3) && isplustwo(p2) || isplusone(p2) && isplustwo(p3))) {
              push(car(cdr(p1)));
              restore();
              return;
            }
          }
          if (expanding && isadd(p1)) {
            p1 = cdr(p1);
            push(zero);
            while (iscons(p1)) {
              push(car(p1));
              push(p2);
              push(p3);
              transpose();
              add();
              p1 = cdr(p1);
            }
            restore();
            return;
          }
          if (expanding && ismultiply(p1)) {
            p1 = cdr(p1);
            push(one);
            while (iscons(p1)) {
              push(car(p1));
              push(p2);
              push(p3);
              transpose();
              multiply();
              p1 = cdr(p1);
            }
            restore();
            return;
          }
          if (expanding && isinnerordot(p1)) {
            p1 = cdr(p1);
            accumulator = [];
            while (iscons(p1)) {
              accumulator.push([car(p1), p2, p3]);
              p1 = cdr(p1);
            }
            for (eachEntry = m1 = ref3 = accumulator.length - 1; ref3 <= 0 ? m1 <= 0 : m1 >= 0; eachEntry = ref3 <= 0 ? ++m1 : --m1) {
              push(accumulator[eachEntry][0]);
              push(accumulator[eachEntry][1]);
              push(accumulator[eachEntry][2]);
              transpose();
              if (eachEntry !== accumulator.length - 1) {
                inner();
              }
            }
            restore();
            return;
          }
          if (!istensor(p1)) {
            if (!isZeroAtomOrTensor(p1)) {
              push_symbol(TRANSPOSE);
              push(p1);
              if ((!isplusone(p2) || !isplustwo(p3)) && (!isplusone(p3) || !isplustwo(p2))) {
                push(p2);
                push(p3);
                list(4);
              } else {
                list(2);
              }
              restore();
              return;
            }
            push(zero);
            restore();
            return;
          }
          ndim = p1.tensor.ndim;
          nelem = p1.tensor.nelem;
          if (ndim === 1) {
            push(p1);
            restore();
            return;
          }
          push(p2);
          l8 = pop_integer();
          push(p3);
          m3 = pop_integer();
          if (l8 < 1 || l8 > ndim || m3 < 1 || m3 > ndim) {
            stop("transpose: index out of range");
          }
          l8--;
          m3--;
          p2 = alloc_tensor(nelem);
          p2.tensor.ndim = ndim;
          for (i5 = n1 = 0, ref4 = ndim; 0 <= ref4 ? n1 < ref4 : n1 > ref4; i5 = 0 <= ref4 ? ++n1 : --n1) {
            p2.tensor.dim[i5] = p1.tensor.dim[i5];
          }
          p2.tensor.dim[l8] = p1.tensor.dim[m3];
          p2.tensor.dim[m3] = p1.tensor.dim[l8];
          a4 = p1.tensor.elem;
          b2 = p2.tensor.elem;
          for (i5 = o1 = 0, ref5 = ndim; 0 <= ref5 ? o1 < ref5 : o1 > ref5; i5 = 0 <= ref5 ? ++o1 : --o1) {
            ai[i5] = 0;
            an[i5] = p1.tensor.dim[i5];
          }
          for (i5 = q1 = 0, ref6 = nelem; 0 <= ref6 ? q1 < ref6 : q1 > ref6; i5 = 0 <= ref6 ? ++q1 : --q1) {
            t5 = ai[l8];
            ai[l8] = ai[m3];
            ai[m3] = t5;
            t5 = an[l8];
            an[l8] = an[m3];
            an[m3] = t5;
            k2 = 0;
            for (j2 = r1 = 0, ref7 = ndim; 0 <= ref7 ? r1 < ref7 : r1 > ref7; j2 = 0 <= ref7 ? ++r1 : --r1) {
              k2 = k2 * an[j2] + ai[j2];
            }
            t5 = ai[l8];
            ai[l8] = ai[m3];
            ai[m3] = t5;
            t5 = an[l8];
            an[l8] = an[m3];
            an[m3] = t5;
            b2[k2] = a4[i5];
            for (j2 = s1 = ref8 = ndim - 1; ref8 <= 0 ? s1 <= 0 : s1 >= 0; j2 = ref8 <= 0 ? ++s1 : --s1) {
              if (++ai[j2] < an[j2]) {
                break;
              }
              ai[j2] = 0;
            }
          }
          push(p2);
          return restore();
        };
        Eval_user_function = function() {
          var bodyAndFormalArguments, h5;
          if (DEBUG) {
            console.log("Eval_user_function evaluating: " + car(p1));
          }
          if (car(p1) === symbol(SYMBOL_D) && get_binding(symbol(SYMBOL_D)) === symbol(SYMBOL_D)) {
            Eval_derivative();
            return;
          }
          push(car(p1));
          Eval();
          bodyAndFormalArguments = pop();
          if (isNumericAtom(bodyAndFormalArguments)) {
            stop("expected function invocation, found multiplication instead. Use '*' symbol explicitly for multiplication.");
          } else if (istensor(bodyAndFormalArguments)) {
            stop("expected function invocation, found tensor product instead. Use 'dot/inner' explicitly.");
          } else if (isstr(bodyAndFormalArguments)) {
            stop("expected function, found string instead.");
          }
          p3 = car(cdr(bodyAndFormalArguments));
          p4 = car(cdr(cdr(bodyAndFormalArguments)));
          p5 = cdr(p1);
          if (car(bodyAndFormalArguments) !== symbol(FUNCTION) || bodyAndFormalArguments === car(p1)) {
            h5 = tos;
            push(bodyAndFormalArguments);
            p1 = p5;
            while (iscons(p1)) {
              push(car(p1));
              Eval();
              p1 = cdr(p1);
            }
            list(tos - h5);
            return;
          }
          p1 = p4;
          p2 = p5;
          h5 = tos;
          while (iscons(p1) && iscons(p2)) {
            push(car(p1));
            push(car(p2));
            p1 = cdr(p1);
            p2 = cdr(p2);
          }
          list(tos - h5);
          p6 = pop();
          push(p3);
          if (iscons(p6)) {
            push(p6);
            rewrite_args();
          }
          return Eval();
        };
        rewrite_args = function() {
          var h5, n9;
          n9 = 0;
          save();
          p2 = pop();
          p1 = pop();
          if (istensor(p1)) {
            n9 = rewrite_args_tensor();
            restore();
            return n9;
          }
          if (iscons(p1)) {
            h5 = tos;
            if (car(p1) === car(p2)) {
              push_symbol(EVAL);
              push(car(cdr(p2)));
              list(2);
            } else {
              push(car(p1));
            }
            p1 = cdr(p1);
            while (iscons(p1)) {
              push(car(p1));
              push(p2);
              n9 += rewrite_args();
              p1 = cdr(p1);
            }
            list(tos - h5);
            restore();
            return n9;
          }
          if (!issymbol(p1)) {
            push(p1);
            restore();
            return 0;
          }
          p3 = p2;
          while (iscons(p3)) {
            if (p1 === car(p3)) {
              push(cadr(p3));
              restore();
              return 1;
            }
            p3 = cddr(p3);
          }
          p3 = get_binding(p1);
          push(p3);
          if (p1 !== p3) {
            push(p2);
            n9 = rewrite_args();
            if (n9 === 0) {
              pop();
              push(p1);
            }
          }
          restore();
          return n9;
        };
        rewrite_args_tensor = function() {
          var i5, l1, n9, ref2;
          n9 = 0;
          i5 = 0;
          push(p1);
          copy_tensor();
          p1 = pop();
          for (i5 = l1 = 0, ref2 = p1.tensor.nelem; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
            push(p1.tensor.elem[i5]);
            push(p2);
            n9 += rewrite_args();
            p1.tensor.elem[i5] = pop();
          }
          check_tensor_dimensions(p1);
          push(p1);
          return n9;
        };
        Eval_zero = function() {
          var i5, k2, l1, m3, m1, n9, ref2, ref3;
          i5 = 0;
          k2 = [];
          m3 = 0;
          n9 = 0;
          for (i5 = l1 = 0, ref2 = MAXDIM; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
            k2[i5] = 0;
          }
          m3 = 1;
          n9 = 0;
          p2 = cdr(p1);
          while (iscons(p2)) {
            push(car(p2));
            Eval();
            i5 = pop_integer();
            if (i5 < 1 || isNaN(i5)) {
              push(zero);
              return;
            }
            m3 *= i5;
            k2[n9++] = i5;
            p2 = cdr(p2);
          }
          if (n9 === 0) {
            push(zero);
            return;
          }
          p1 = alloc_tensor(m3);
          p1.tensor.ndim = n9;
          for (i5 = m1 = 0, ref3 = n9; 0 <= ref3 ? m1 < ref3 : m1 > ref3; i5 = 0 <= ref3 ? ++m1 : --m1) {
            p1.tensor.dim[i5] = k2[i5];
          }
          return push(p1);
        };
        allocatedId = 0;
        alloc_tensor = function(nelem) {
          var i5, l1, p11, ref2;
          i5 = 0;
          p11 = new U();
          p11.k = TENSOR;
          p11.tensor = new tensor();
          p11.tensor.nelem = nelem;
          for (i5 = l1 = 0, ref2 = nelem; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
            p11.tensor.elem[i5] = zero;
          }
          p11.tensor.allocatedId = allocatedId;
          allocatedId++;
          check_tensor_dimensions(p11);
          return p11;
        };
        Find = function(p11, q) {
          var i5, l1, ref2;
          i5 = 0;
          if (equal(p11, q)) {
            return 1;
          }
          if (istensor(p11)) {
            for (i5 = l1 = 0, ref2 = p11.tensor.nelem; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
              if (Find(p11.tensor.elem[i5], q)) {
                return 1;
              }
            }
            return 0;
          }
          while (iscons(p11)) {
            if (Find(car(p11), q)) {
              return 1;
            }
            p11 = cdr(p11);
          }
          return 0;
        };
        findPossibleClockForm = function(p11) {
          var i5, l1, ref2;
          i5 = 0;
          if (isimaginaryunit(p11)) {
            return 0;
          }
          if (car(p11) === symbol(POWER) && !isinteger(caddr(p1))) {
            if (Find(cadr(p11), imaginaryunit)) {
              return 1;
            }
          }
          if (car(p11) === symbol(POWER) && equaln(cadr(p11), -1) && !isinteger(caddr(p1))) {
            return 1;
          }
          if (istensor(p11)) {
            for (i5 = l1 = 0, ref2 = p11.tensor.nelem; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
              if (findPossibleClockForm(p11.tensor.elem[i5])) {
                return 1;
              }
            }
            return 0;
          }
          while (iscons(p11)) {
            if (findPossibleClockForm(car(p11))) {
              return 1;
            }
            p11 = cdr(p11);
          }
          return 0;
        };
        findPossibleExponentialForm = function(p11) {
          var i5, l1, ref2;
          i5 = 0;
          if (car(p11) === symbol(POWER) && cadr(p11) === symbol(E)) {
            return Find(caddr(p11), imaginaryunit);
          }
          if (istensor(p11)) {
            for (i5 = l1 = 0, ref2 = p11.tensor.nelem; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
              if (findPossibleExponentialForm(p11.tensor.elem[i5])) {
                return 1;
              }
            }
            return 0;
          }
          while (iscons(p11)) {
            if (findPossibleExponentialForm(car(p11))) {
              return 1;
            }
            p11 = cdr(p11);
          }
          return 0;
        };
        $.Find = Find;
        init = function() {
          var i5, l1, ref2;
          i5 = 0;
          flag = 0;
          reset_after_error();
          chainOfUserSymbolsNotFunctionsBeingEvaluated = [];
          if (flag) {
            return;
          }
          flag = 1;
          for (i5 = l1 = 0, ref2 = NSYM; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
            symtab[i5] = new U();
            symtab[i5].k = SYM;
            binding[i5] = symtab[i5];
            isSymbolReclaimable[i5] = false;
          }
          return defn();
        };
        defn_str = [
          'version="' + version + '"',
          "e=exp(1)",
          "i=sqrt(-1)",
          "autoexpand=1",
          "assumeRealVariables=1",
          "trange=[-pi,pi]",
          "xrange=[-10,10]",
          "yrange=[-10,10]",
          "last=0",
          "trace=0",
          "forceFixedPrintout=1",
          "maxFixedPrintoutDigits=6",
          "printLeaveEAlone=1",
          "printLeaveXAlone=0",
          // cross definition
          "cross(u,v)=[u[2]*v[3]-u[3]*v[2],u[3]*v[1]-u[1]*v[3],u[1]*v[2]-u[2]*v[1]]",
          // curl definition
          "curl(v)=[d(v[3],y)-d(v[2],z),d(v[1],z)-d(v[3],x),d(v[2],x)-d(v[1],y)]",
          // div definition
          "div(v)=d(v[1],x)+d(v[2],y)+d(v[3],z)",
          // Note that we use the mathematics / Javascript / Mathematica
          // convention that "log" is indeed the natural logarithm.
          // In engineering, biology, astronomy, "log" can stand instead
          // for the "common" logarithm i.e. base 10. Also note that Google
          // calculations use log for the common logarithm.
          "ln(x)=log(x)"
        ];
        defn = function() {
          var definitionOfInterest, defn_i, l1, originalCodeGen, ref2;
          p0 = symbol(NIL);
          p1 = symbol(NIL);
          p2 = symbol(NIL);
          p3 = symbol(NIL);
          p4 = symbol(NIL);
          p5 = symbol(NIL);
          p6 = symbol(NIL);
          p7 = symbol(NIL);
          p8 = symbol(NIL);
          p9 = symbol(NIL);
          std_symbol("abs", ABS);
          std_symbol("add", ADD);
          std_symbol("adj", ADJ);
          std_symbol("and", AND);
          std_symbol("approxratio", APPROXRATIO);
          std_symbol("arccos", ARCCOS);
          std_symbol("arccosh", ARCCOSH);
          std_symbol("arcsin", ARCSIN);
          std_symbol("arcsinh", ARCSINH);
          std_symbol("arctan", ARCTAN);
          std_symbol("arctanh", ARCTANH);
          std_symbol("arg", ARG);
          std_symbol("atomize", ATOMIZE);
          std_symbol("besselj", BESSELJ);
          std_symbol("bessely", BESSELY);
          std_symbol("binding", BINDING);
          std_symbol("binomial", BINOMIAL);
          std_symbol("ceiling", CEILING);
          std_symbol("check", CHECK);
          std_symbol("choose", CHOOSE);
          std_symbol("circexp", CIRCEXP);
          std_symbol("clear", CLEAR);
          std_symbol("clearall", CLEARALL);
          std_symbol("clearpatterns", CLEARPATTERNS);
          std_symbol("clock", CLOCK);
          std_symbol("coeff", COEFF);
          std_symbol("cofactor", COFACTOR);
          std_symbol("condense", CONDENSE);
          std_symbol("conj", CONJ);
          std_symbol("contract", CONTRACT);
          std_symbol("cos", COS);
          std_symbol("cosh", COSH);
          std_symbol("decomp", DECOMP);
          std_symbol("defint", DEFINT);
          std_symbol("deg", DEGREE);
          std_symbol("denominator", DENOMINATOR);
          std_symbol("det", DET);
          std_symbol("derivative", DERIVATIVE);
          std_symbol("dim", DIM);
          std_symbol("dirac", DIRAC);
          std_symbol("divisors", DIVISORS);
          std_symbol("do", DO);
          std_symbol("dot", DOT);
          std_symbol("draw", DRAW);
          std_symbol("dsolve", DSOLVE);
          std_symbol("erf", ERF);
          std_symbol("erfc", ERFC);
          std_symbol("eigen", EIGEN);
          std_symbol("eigenval", EIGENVAL);
          std_symbol("eigenvec", EIGENVEC);
          std_symbol("eval", EVAL);
          std_symbol("exp", EXP);
          std_symbol("expand", EXPAND);
          std_symbol("expcos", EXPCOS);
          std_symbol("expsin", EXPSIN);
          std_symbol("factor", FACTOR);
          std_symbol("factorial", FACTORIAL);
          std_symbol("factorpoly", FACTORPOLY);
          std_symbol("filter", FILTER);
          std_symbol("float", FLOATF);
          std_symbol("floor", FLOOR);
          std_symbol("for", FOR);
          std_symbol("function", FUNCTION);
          std_symbol("Gamma", GAMMA);
          std_symbol("gcd", GCD);
          std_symbol("hermite", HERMITE);
          std_symbol("hilbert", HILBERT);
          std_symbol("imag", IMAG);
          std_symbol("component", INDEX);
          std_symbol("inner", INNER);
          std_symbol("integral", INTEGRAL);
          std_symbol("inv", INV);
          std_symbol("invg", INVG);
          std_symbol("isinteger", ISINTEGER);
          std_symbol("isprime", ISPRIME);
          std_symbol("laguerre", LAGUERRE);
          std_symbol("lcm", LCM);
          std_symbol("leading", LEADING);
          std_symbol("legendre", LEGENDRE);
          std_symbol("log", LOG);
          std_symbol("lookup", LOOKUP);
          std_symbol("mod", MOD);
          std_symbol("multiply", MULTIPLY);
          std_symbol("not", NOT);
          std_symbol("nroots", NROOTS);
          std_symbol("number", NUMBER);
          std_symbol("numerator", NUMERATOR);
          std_symbol("operator", OPERATOR);
          std_symbol("or", OR);
          std_symbol("outer", OUTER);
          std_symbol("pattern", PATTERN);
          std_symbol("patternsinfo", PATTERNSINFO);
          std_symbol("polar", POLAR);
          std_symbol("power", POWER);
          std_symbol("prime", PRIME);
          std_symbol("print", PRINT);
          std_symbol("print2dascii", PRINT2DASCII);
          std_symbol("printcomputer", PRINTFULL);
          std_symbol("printlatex", PRINTLATEX);
          std_symbol("printlist", PRINTLIST);
          std_symbol("printhuman", PRINTPLAIN);
          std_symbol("printLeaveEAlone", PRINT_LEAVE_E_ALONE);
          std_symbol("printLeaveXAlone", PRINT_LEAVE_X_ALONE);
          std_symbol("product", PRODUCT);
          std_symbol("quote", QUOTE);
          std_symbol("quotient", QUOTIENT);
          std_symbol("rank", RANK);
          std_symbol("rationalize", RATIONALIZE);
          std_symbol("real", REAL);
          std_symbol("rect", YYRECT);
          std_symbol("roots", ROOTS);
          std_symbol("round", ROUND);
          std_symbol("equals", SETQ);
          std_symbol("sgn", SGN);
          std_symbol("silentpattern", SILENTPATTERN);
          std_symbol("simplify", SIMPLIFY);
          std_symbol("sin", SIN);
          std_symbol("sinh", SINH);
          std_symbol("shape", SHAPE);
          std_symbol("sqrt", SQRT);
          std_symbol("stop", STOP);
          std_symbol("subst", SUBST);
          std_symbol("sum", SUM);
          std_symbol("symbolsinfo", SYMBOLSINFO);
          std_symbol("tan", TAN);
          std_symbol("tanh", TANH);
          std_symbol("taylor", TAYLOR);
          std_symbol("test", TEST);
          std_symbol("testeq", TESTEQ);
          std_symbol("testge", TESTGE);
          std_symbol("testgt", TESTGT);
          std_symbol("testle", TESTLE);
          std_symbol("testlt", TESTLT);
          std_symbol("transpose", TRANSPOSE);
          std_symbol("unit", UNIT);
          std_symbol("zero", ZERO);
          std_symbol("nil", NIL);
          std_symbol("autoexpand", AUTOEXPAND);
          std_symbol("bake", BAKE);
          std_symbol("assumeRealVariables", ASSUME_REAL_VARIABLES);
          std_symbol("last", LAST);
          std_symbol("lastprint", LAST_PRINT);
          std_symbol("last2dasciiprint", LAST_2DASCII_PRINT);
          std_symbol("lastfullprint", LAST_FULL_PRINT);
          std_symbol("lastlatexprint", LAST_LATEX_PRINT);
          std_symbol("lastlistprint", LAST_LIST_PRINT);
          std_symbol("lastplainprint", LAST_PLAIN_PRINT);
          std_symbol("trace", TRACE);
          std_symbol("forceFixedPrintout", FORCE_FIXED_PRINTOUT);
          std_symbol("maxFixedPrintoutDigits", MAX_FIXED_PRINTOUT_DIGITS);
          std_symbol("~", YYE);
          std_symbol("$DRAWX", DRAWX);
          std_symbol("$METAA", METAA);
          std_symbol("$METAB", METAB);
          std_symbol("$METAX", METAX);
          std_symbol("$SECRETX", SECRETX);
          std_symbol("version", VERSION);
          std_symbol("pi", PI);
          std_symbol("a", SYMBOL_A);
          std_symbol("b", SYMBOL_B);
          std_symbol("c", SYMBOL_C);
          std_symbol("d", SYMBOL_D);
          std_symbol("i", SYMBOL_I);
          std_symbol("j", SYMBOL_J);
          std_symbol("n", SYMBOL_N);
          std_symbol("r", SYMBOL_R);
          std_symbol("s", SYMBOL_S);
          std_symbol("t", SYMBOL_T);
          std_symbol("x", SYMBOL_X);
          std_symbol("y", SYMBOL_Y);
          std_symbol("z", SYMBOL_Z);
          std_symbol("I", SYMBOL_IDENTITY_MATRIX);
          std_symbol("a_", SYMBOL_A_UNDERSCORE);
          std_symbol("b_", SYMBOL_B_UNDERSCORE);
          std_symbol("x_", SYMBOL_X_UNDERSCORE);
          std_symbol("$C1", C1);
          std_symbol("$C2", C2);
          std_symbol("$C3", C3);
          std_symbol("$C4", C4);
          std_symbol("$C5", C5);
          std_symbol("$C6", C6);
          defineSomeHandyConstants();
          originalCodeGen = codeGen;
          codeGen = false;
          for (defn_i = l1 = 0, ref2 = defn_str.length; 0 <= ref2 ? l1 < ref2 : l1 > ref2; defn_i = 0 <= ref2 ? ++l1 : --l1) {
            definitionOfInterest = defn_str[defn_i];
            scan(definitionOfInterest);
            if (DEBUG) {
              console.log("... evaling " + definitionOfInterest);
              console.log("top of stack:");
              console.log(print_list(stack[tos - 1]));
            }
            Eval();
            pop();
          }
          return codeGen = originalCodeGen;
        };
        defineSomeHandyConstants = function() {
          zero = new_integer(0);
          one = new_integer(1);
          push_double(1);
          one_as_double = pop();
          push_symbol(POWER);
          if (DEBUG) {
            console.log(print_list(stack[tos - 1]));
          }
          push_integer(-1);
          if (DEBUG) {
            console.log(print_list(stack[tos - 1]));
          }
          push_rational(1, 2);
          if (DEBUG) {
            console.log(print_list(stack[tos - 1]));
          }
          list(3);
          if (DEBUG) {
            console.log(print_list(stack[tos - 1]));
          }
          return imaginaryunit = pop();
        };
        mcmp = function(a4, b2) {
          return a4.compare(b2);
        };
        mcmpint = function(a4, n9) {
          var b2, t5;
          b2 = bigInt(n9);
          t5 = mcmp(a4, b2);
          return t5;
        };
        strcmp = function(str1, str2) {
          if (str1 === str2) {
            return 0;
          } else if (str1 > str2) {
            return 1;
          } else {
            return -1;
          }
        };
        doubleToReasonableString = function(d3) {
          var maxFixedPrintoutDigits, stringRepresentation;
          if (codeGen) {
            return "" + d3;
          }
          if (isZeroAtomOrTensor(get_binding(symbol(FORCE_FIXED_PRINTOUT)))) {
            stringRepresentation = "" + d3;
            if (printMode === PRINTMODE_LATEX) {
              if (/\d*\.\d*e.*/gm.test(stringRepresentation)) {
                stringRepresentation = stringRepresentation.replace(/e(.*)/gm, "\\mathrm{e}{$1}");
              } else {
                stringRepresentation = stringRepresentation.replace(/(\d+)e(.*)/gm, "$1.0\\mathrm{e}{$2}");
              }
            } else {
              if (/\d*\.\d*e.*/gm.test(stringRepresentation)) {
                stringRepresentation = stringRepresentation.replace(/e(.*)/gm, "*10^($1)");
              } else {
                stringRepresentation = stringRepresentation.replace(/(\d+)e(.*)/gm, "$1.0*10^($2)");
              }
            }
          } else {
            push(get_binding(symbol(MAX_FIXED_PRINTOUT_DIGITS)));
            maxFixedPrintoutDigits = pop_integer();
            stringRepresentation = "" + d3.toFixed(maxFixedPrintoutDigits);
            stringRepresentation = stringRepresentation.replace(/(\.\d*?[1-9])0+$/gm, "$1");
            stringRepresentation = stringRepresentation.replace(/\.0+$/gm, "");
            if (stringRepresentation.indexOf(".") === -1) {
              stringRepresentation += ".0";
            }
            if (parseFloat(stringRepresentation) !== d3) {
              stringRepresentation = d3.toFixed(maxFixedPrintoutDigits) + "...";
            }
          }
          return stringRepresentation;
        };
        clear_term = function() {
        };
        isspace = function(s7) {
          if (s7 == null) {
            return false;
          }
          return s7 === " " || s7 === "	" || s7 === "\n" || s7 === "\v" || s7 === "\f" || s7 === "\r";
        };
        isdigit = function(str) {
          if (str == null) {
            return false;
          }
          return /^\d+$/.test(str);
        };
        isalpha = function(str) {
          if (str == null) {
            return false;
          }
          return str.search(/[^A-Za-z]/) === -1;
        };
        isalphaOrUnderscore = function(str) {
          if (str == null) {
            return false;
          }
          return str.search(/[^A-Za-z_]/) === -1;
        };
        isunderscore = function(str) {
          if (str == null) {
            return false;
          }
          return str.search(/_/) === -1;
        };
        isalnumorunderscore = function(str) {
          if (str == null) {
            return false;
          }
          return isalphaOrUnderscore(str) || isdigit(str);
        };
        count = function(p11) {
          var n9;
          if (iscons(p11)) {
            n9 = 0;
            while (iscons(p11)) {
              n9 += count(car(p11)) + 1;
              p11 = cdr(p11);
            }
          } else {
            n9 = 1;
          }
          return n9;
        };
        countOccurrencesOfSymbol = function(needle, p11) {
          var n9;
          n9 = 0;
          if (iscons(p11)) {
            while (iscons(p11)) {
              n9 += countOccurrencesOfSymbol(needle, car(p11));
              p11 = cdr(p11);
            }
          } else {
            if (equal(needle, p11)) {
              n9 = 1;
            }
          }
          return n9;
        };
        countsize = function(p11) {
          var i5, l1, n9, ref2;
          n9 = 0;
          if (istensor(p11)) {
            for (i5 = l1 = 0, ref2 = p11.tensor.nelem; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
              n9 += p11.tensor.elem[i5];
            }
          } else if (iscons(p11)) {
            while (iscons(p11)) {
              n9 += count(car(p11)) + 1;
              p11 = cdr(p11);
            }
          } else {
            n9 = 1;
          }
          return n9;
        };
        stop = function(s7) {
          var message;
          errorMessage += "Stop: ";
          errorMessage += s7;
          message = errorMessage;
          errorMessage = "";
          moveTos(0);
          throw new Error(message);
        };
        findDependenciesInScript = function(stringToBeParsed, dontGenerateCode) {
          var allReturnedLatexStrings, allReturnedPlainStrings, bodyForReadableSummaryOfGeneratedCode, cyclesDescriptions, deQuotedDep, dependencyInfo, eachDependency, error, generatedBody, generatedCode, i5, indexOfEachReplacement, indexOfPartRemainingToBeParsed, inited2, key, l1, len, len1, len2, len3, len4, len5, len6, len7, m1, n9, n1, newUserSymbol, o1, origPrintMode, originalUserSymbol, parameters, q1, r1, readableSummaryOfGeneratedCode, recursedDependencies, ref2, replacementsFrom, replacementsTo, s1, scriptEvaluation, stringToBeRun, t1, testableString, timeStartFromAlgebra, toBePrinted, u1, userVariablesMentioned, value, variablesWithCycles;
          if (DEBUG) {
            console.log("stringToBeParsed: " + stringToBeParsed);
          }
          timeStartFromAlgebra = (/* @__PURE__ */ new Date()).getTime();
          inited2 = true;
          codeGen = true;
          symbolsDependencies = {};
          symbolsHavingReassignments = [];
          symbolsInExpressionsWithoutAssignments = [];
          patternHasBeenFound = false;
          indexOfPartRemainingToBeParsed = 0;
          allReturnedPlainStrings = "";
          allReturnedLatexStrings = "";
          n9 = 0;
          dependencyInfo = {
            affectsVariables: [],
            affectedBy: []
          };
          stringToBeRun = stringToBeParsed;
          while (1) {
            try {
              errorMessage = "";
              check_stack();
              if (DEBUG) {
                console.log("findDependenciesInScript: scanning");
              }
              n9 = scan(stringToBeParsed.substring(indexOfPartRemainingToBeParsed));
              if (DEBUG) {
                console.log("scanned");
              }
              pop();
              check_stack();
            } catch (error1) {
              error = error1;
              if (PRINTOUTRESULT) {
                console.log(error);
              }
              errorMessage = error + "";
              reset_after_error();
              break;
            }
            if (n9 === 0) {
              break;
            }
            indexOfPartRemainingToBeParsed += n9;
          }
          testableString = "";
          if (DEBUG) {
            console.log("all local dependencies ----------------");
          }
          testableString += "All local dependencies: ";
          for (key in symbolsDependencies) {
            value = symbolsDependencies[key];
            if (DEBUG) {
              console.log("variable " + key + " depends on: ");
            }
            dependencyInfo.affectsVariables.push(key);
            testableString += " variable " + key + " depends on: ";
            for (l1 = 0, len = value.length; l1 < len; l1++) {
              i5 = value[l1];
              if (DEBUG) {
                console.log("    " + i5);
              }
              if (i5[0] !== "'") {
                dependencyInfo.affectedBy.push(i5);
              }
              testableString += i5 + ", ";
            }
            testableString += "; ";
          }
          testableString += ". ";
          if (DEBUG) {
            console.log("Symbols with reassignments ----------------");
          }
          testableString += "Symbols with reassignments: ";
          for (m1 = 0, len1 = symbolsHavingReassignments.length; m1 < len1; m1++) {
            key = symbolsHavingReassignments[m1];
            if (dependencyInfo.affectedBy.indexOf(key) === -1) {
              dependencyInfo.affectedBy.push(key);
              testableString += key + ", ";
            }
          }
          testableString += ". ";
          if (DEBUG) {
            console.log("Symbols in expressions without assignments ----------------");
          }
          testableString += "Symbols in expressions without assignments: ";
          for (n1 = 0, len2 = symbolsInExpressionsWithoutAssignments.length; n1 < len2; n1++) {
            key = symbolsInExpressionsWithoutAssignments[n1];
            if (dependencyInfo.affectedBy.indexOf(key) === -1) {
              dependencyInfo.affectedBy.push(key);
              testableString += key + ", ";
            }
          }
          testableString += ". ";
          dependencyInfo.affectedBy.push("PATTERN_DEPENDENCY");
          if (patternHasBeenFound) {
            dependencyInfo.affectsVariables.push("PATTERN_DEPENDENCY");
            testableString += " - PATTERN_DEPENDENCY inserted - ";
          }
          if (DEBUG) {
            console.log("All dependencies recursively ----------------");
          }
          testableString += "All dependencies recursively: ";
          scriptEvaluation = ["", ""];
          generatedCode = "";
          readableSummaryOfGeneratedCode = "";
          if (errorMessage === "" && !dontGenerateCode) {
            try {
              allReturnedPlainStrings = "";
              allReturnedLatexStrings = "";
              scriptEvaluation = run(stringToBeParsed, true);
              allReturnedPlainStrings = "";
              allReturnedLatexStrings = "";
            } catch (error1) {
              error = error1;
              if (PRINTOUTRESULT) {
                console.log(error);
              }
              errorMessage = error + "";
              init();
            }
            if (errorMessage === "") {
              for (key in symbolsDependencies) {
                codeGen = true;
                if (DEBUG) {
                  console.log("  variable " + key + " is: " + get_binding(usr_symbol(key)).toString());
                }
                codeGen = false;
                if (DEBUG) {
                  console.log("  variable " + key + " depends on: ");
                }
                testableString += " variable " + key + " depends on: ";
                recursedDependencies = [];
                variablesWithCycles = [];
                cyclesDescriptions = [];
                recursiveDependencies(key, recursedDependencies, [], variablesWithCycles, [], cyclesDescriptions);
                for (o1 = 0, len3 = variablesWithCycles.length; o1 < len3; o1++) {
                  i5 = variablesWithCycles[o1];
                  if (DEBUG) {
                    console.log("    --> cycle through " + i5);
                  }
                }
                for (q1 = 0, len4 = recursedDependencies.length; q1 < len4; q1++) {
                  i5 = recursedDependencies[q1];
                  if (DEBUG) {
                    console.log("    " + i5);
                  }
                  testableString += i5 + ", ";
                }
                testableString += "; ";
                for (r1 = 0, len5 = cyclesDescriptions.length; r1 < len5; r1++) {
                  i5 = cyclesDescriptions[r1];
                  testableString += " " + i5 + ", ";
                }
                if (DEBUG) {
                  console.log("  code generation:" + key + " is: " + get_binding(usr_symbol(key)).toString());
                }
                push(get_binding(usr_symbol(key)));
                replacementsFrom = [];
                replacementsTo = [];
                for (s1 = 0, len6 = recursedDependencies.length; s1 < len6; s1++) {
                  eachDependency = recursedDependencies[s1];
                  if (eachDependency[0] === "'") {
                    deQuotedDep = eachDependency.substring(1);
                    originalUserSymbol = usr_symbol(deQuotedDep);
                    newUserSymbol = usr_symbol("AVOID_BINDING_TO_EXTERNAL_SCOPE_VALUE" + deQuotedDep);
                    replacementsFrom.push(originalUserSymbol);
                    replacementsTo.push(newUserSymbol);
                    push(originalUserSymbol);
                    push(newUserSymbol);
                    subst();
                    if (DEBUG) {
                      console.log("after substitution: " + stack[tos - 1]);
                    }
                  }
                }
                try {
                  simplifyForCodeGeneration();
                } catch (error1) {
                  error = error1;
                  if (PRINTOUTRESULT) {
                    console.log(error);
                  }
                  errorMessage = error + "";
                  init();
                }
                for (indexOfEachReplacement = t1 = 0, ref2 = replacementsFrom.length; 0 <= ref2 ? t1 < ref2 : t1 > ref2; indexOfEachReplacement = 0 <= ref2 ? ++t1 : --t1) {
                  push(replacementsTo[indexOfEachReplacement]);
                  push(replacementsFrom[indexOfEachReplacement]);
                  subst();
                }
                clearRenamedVariablesToAvoidBindingToExternalScope();
                if (errorMessage === "") {
                  toBePrinted = pop();
                  userVariablesMentioned = [];
                  collectUserSymbols(toBePrinted, userVariablesMentioned);
                  allReturnedPlainStrings = "";
                  allReturnedLatexStrings = "";
                  codeGen = true;
                  generatedBody = toBePrinted.toString();
                  codeGen = false;
                  origPrintMode = printMode;
                  printMode = PRINTMODE_LATEX;
                  bodyForReadableSummaryOfGeneratedCode = toBePrinted.toString();
                  printMode = origPrintMode;
                  if (variablesWithCycles.indexOf(key) !== -1) {
                    generatedCode += "// " + key + " is part of a cyclic dependency, no code generated.";
                    readableSummaryOfGeneratedCode += "#" + key + " is part of a cyclic dependency, no code generated.";
                  } else {
                    userVariablesMentioned = userVariablesMentioned.filter(function(x2) {
                      return predefinedSymbolsInGlobalScope_doNotTrackInDependencies.indexOf(x2 + "") === -1;
                    });
                    userVariablesMentioned = userVariablesMentioned.filter(function(x2) {
                      return recursedDependencies.indexOf(x2 + "") !== -1 || recursedDependencies.indexOf("'" + x2) !== -1;
                    });
                    if (userVariablesMentioned.length !== 0) {
                      parameters = "(";
                      for (u1 = 0, len7 = userVariablesMentioned.length; u1 < len7; u1++) {
                        i5 = userVariablesMentioned[u1];
                        if (i5.printname !== key) {
                          parameters += i5.printname + ", ";
                        }
                      }
                      parameters = parameters.replace(/, $/gm, "");
                      parameters += ")";
                      generatedCode += key + " = function " + parameters + " { return ( " + generatedBody + " ); }";
                      readableSummaryOfGeneratedCode += key + parameters + " = " + bodyForReadableSummaryOfGeneratedCode;
                    } else {
                      generatedCode += key + " = " + generatedBody + ";";
                      readableSummaryOfGeneratedCode += key + " = " + bodyForReadableSummaryOfGeneratedCode;
                    }
                  }
                  generatedCode += "\n";
                  readableSummaryOfGeneratedCode += "\n";
                  if (DEBUG) {
                    console.log("    " + generatedCode);
                  }
                }
              }
            }
          }
          generatedCode = generatedCode.replace(/\n$/gm, "");
          readableSummaryOfGeneratedCode = readableSummaryOfGeneratedCode.replace(/\n$/gm, "");
          symbolsDependencies = {};
          symbolsHavingReassignments = [];
          patternHasBeenFound = false;
          symbolsInExpressionsWithoutAssignments = [];
          if (DEBUG) {
            console.log("testable string: " + testableString);
          }
          if (TIMING_DEBUGS) {
            console.log("findDependenciesInScript time for: " + stringToBeRun + " : " + ((/* @__PURE__ */ new Date()).getTime() - timeStartFromAlgebra) + "ms");
          }
          return [testableString, scriptEvaluation[0], generatedCode, readableSummaryOfGeneratedCode, scriptEvaluation[1], errorMessage, dependencyInfo];
        };
        recursiveDependencies = function(variableToBeChecked, arrayWhereDependenciesWillBeAdded, variablesAlreadyFleshedOut, variablesWithCycles, chainBeingChecked, cyclesDescriptions) {
          var cyclesDescription, i5, k2, l1, len, len1, m1, ref2;
          variablesAlreadyFleshedOut.push(variableToBeChecked);
          if (symbolsDependencies[chainBeingChecked[chainBeingChecked.length - 1]] != null) {
            if (symbolsDependencies[chainBeingChecked[chainBeingChecked.length - 1]].indexOf("'" + variableToBeChecked) !== -1) {
              if (DEBUG) {
                console.log("can't keep following the chain of " + variableToBeChecked + " because it's actually a variable bound to a parameter");
              }
              if (arrayWhereDependenciesWillBeAdded.indexOf("'" + variableToBeChecked) === -1 && arrayWhereDependenciesWillBeAdded.indexOf(variableToBeChecked) === -1) {
                arrayWhereDependenciesWillBeAdded.push(variableToBeChecked);
              }
              return arrayWhereDependenciesWillBeAdded;
            }
          }
          chainBeingChecked.push(variableToBeChecked);
          if (symbolsDependencies[variableToBeChecked] == null) {
            if (arrayWhereDependenciesWillBeAdded.indexOf(variableToBeChecked) === -1) {
              arrayWhereDependenciesWillBeAdded.push(variableToBeChecked);
            }
            return arrayWhereDependenciesWillBeAdded;
          } else {
            ref2 = symbolsDependencies[variableToBeChecked];
            for (l1 = 0, len = ref2.length; l1 < len; l1++) {
              i5 = ref2[l1];
              if (chainBeingChecked.indexOf(i5) !== -1) {
                if (DEBUG) {
                  console.log("  found cycle:");
                }
                cyclesDescription = "";
                for (m1 = 0, len1 = chainBeingChecked.length; m1 < len1; m1++) {
                  k2 = chainBeingChecked[m1];
                  if (variablesWithCycles.indexOf(k2) === -1) {
                    variablesWithCycles.push(k2);
                  }
                  if (DEBUG) {
                    console.log(k2 + " --> ");
                  }
                  cyclesDescription += k2 + " --> ";
                }
                if (DEBUG) {
                  console.log(" ... then " + i5 + " again");
                }
                cyclesDescription += " ... then " + i5 + " again";
                cyclesDescriptions.push(cyclesDescription);
                if (variablesWithCycles.indexOf(i5) === -1) {
                  variablesWithCycles.push(i5);
                }
              } else {
                recursiveDependencies(i5, arrayWhereDependenciesWillBeAdded, variablesAlreadyFleshedOut, variablesWithCycles, chainBeingChecked, cyclesDescriptions);
                chainBeingChecked.pop();
              }
            }
            return arrayWhereDependenciesWillBeAdded;
          }
        };
        inited = false;
        latexErrorSign = "\\rlap{\\large\\color{red}\\bigtriangleup}{\\ \\ \\tiny\\color{red}!}";
        turnErrorMessageToLatex = function(theErrorMessage) {
          theErrorMessage = theErrorMessage.replace(/\n/g, "");
          theErrorMessage = theErrorMessage.replace(/_/g, "} \\_ \\text{");
          theErrorMessage = theErrorMessage.replace(new RegExp(String.fromCharCode(transpose_unicode), "g"), "}{}^{T}\\text{");
          theErrorMessage = theErrorMessage.replace(new RegExp(String.fromCharCode(dotprod_unicode), "g"), "}\\cdot \\text{");
          theErrorMessage = theErrorMessage.replace("Stop:", "}  \\quad \\text{Stop:");
          theErrorMessage = theErrorMessage.replace("->", "}  \\rightarrow \\text{");
          theErrorMessage = theErrorMessage.replace("?", "}\\enspace " + latexErrorSign + " \\enspace  \\text{");
          theErrorMessage = "$$\\text{" + theErrorMessage.replace(/\n/g, "") + "}$$";
          return theErrorMessage;
        };
        normaliseDots = function(stringToNormalise) {
          stringToNormalise = stringToNormalise.replace(new RegExp(String.fromCharCode(8901), "g"), String.fromCharCode(dotprod_unicode));
          stringToNormalise = stringToNormalise.replace(new RegExp(String.fromCharCode(8226), "g"), String.fromCharCode(dotprod_unicode));
          stringToNormalise = stringToNormalise.replace(new RegExp(String.fromCharCode(12539), "g"), String.fromCharCode(dotprod_unicode));
          stringToNormalise = stringToNormalise.replace(new RegExp(String.fromCharCode(55296), "g"), String.fromCharCode(dotprod_unicode));
          stringToNormalise = stringToNormalise.replace(new RegExp(String.fromCharCode(65381), "g"), String.fromCharCode(dotprod_unicode));
          return stringToNormalise;
        };
        TIMING_DEBUGS = false;
        run = function(stringToBeRun, generateLatex = false) {
          var allReturnedLatexStrings, allReturnedPlainStrings, collectedLatexResult, collectedPlainResult, error, errorWhileExecution, i5, indexOfPartRemainingToBeParsed, n9, stringToBeReturned, theErrorMessage, timeStart, timingDebugWrite;
          timeStart = (/* @__PURE__ */ new Date()).getTime();
          stringToBeRun = normaliseDots(stringToBeRun);
          if (stringToBeRun === "selftest") {
            selftest();
            return;
          }
          if (!inited) {
            inited = true;
            init();
          }
          i5 = 0;
          n9 = 0;
          indexOfPartRemainingToBeParsed = 0;
          allReturnedPlainStrings = "";
          allReturnedLatexStrings = "";
          while (1) {
            try {
              errorMessage = "";
              check_stack();
              n9 = scan(stringToBeRun.substring(indexOfPartRemainingToBeParsed));
              p1 = pop();
              check_stack();
            } catch (error1) {
              error = error1;
              if (PRINTOUTRESULT) {
                console.log(error);
              }
              allReturnedPlainStrings += error.message;
              if (generateLatex) {
                theErrorMessage = turnErrorMessageToLatex(error.message);
                allReturnedLatexStrings += theErrorMessage;
              }
              reset_after_error();
              break;
            }
            if (n9 === 0) {
              break;
            }
            indexOfPartRemainingToBeParsed += n9;
            push(p1);
            errorWhileExecution = false;
            try {
              stringsEmittedByUserPrintouts = "";
              top_level_eval();
              p2 = pop();
              check_stack();
              if (isstr(p2)) {
                if (DEBUG) {
                  console.log(p2.str);
                }
                if (DEBUG) {
                  console.log("\n");
                }
              }
              if (p2 === symbol(NIL)) {
                collectedPlainResult = stringsEmittedByUserPrintouts;
                if (generateLatex) {
                  collectedLatexResult = "$$" + stringsEmittedByUserPrintouts + "$$";
                }
              } else {
                collectedPlainResult = print_expr(p2);
                collectedPlainResult += "\n";
                if (generateLatex) {
                  collectedLatexResult = "$$" + collectLatexStringFromReturnValue(p2) + "$$";
                  if (DEBUG) {
                    console.log("collectedLatexResult: " + collectedLatexResult);
                  }
                }
              }
              allReturnedPlainStrings += collectedPlainResult;
              if (generateLatex) {
                allReturnedLatexStrings += collectedLatexResult;
              }
              if (PRINTOUTRESULT) {
                if (DEBUG) {
                  console.log("printline");
                }
                if (DEBUG) {
                  console.log(collectedPlainResult);
                }
              }
              if (PRINTOUTRESULT) {
                if (DEBUG) {
                  console.log("display:");
                }
                print2dascii(p2);
              }
              if (generateLatex) {
                allReturnedLatexStrings += "\n";
              }
            } catch (error1) {
              error = error1;
              errorWhileExecution = true;
              collectedPlainResult = error.message;
              if (generateLatex) {
                collectedLatexResult = turnErrorMessageToLatex(error.message);
              }
              if (PRINTOUTRESULT) {
                console.log(collectedPlainResult);
              }
              allReturnedPlainStrings += collectedPlainResult;
              if (collectedPlainResult !== "") {
                allReturnedPlainStrings += "\n";
              }
              if (generateLatex) {
                allReturnedLatexStrings += collectedLatexResult;
                allReturnedLatexStrings += "\n";
              }
              init();
            }
          }
          if (allReturnedPlainStrings[allReturnedPlainStrings.length - 1] === "\n") {
            allReturnedPlainStrings = allReturnedPlainStrings.substring(0, allReturnedPlainStrings.length - 1);
          }
          if (generateLatex) {
            if (allReturnedLatexStrings[allReturnedLatexStrings.length - 1] === "\n") {
              allReturnedLatexStrings = allReturnedLatexStrings.substring(0, allReturnedLatexStrings.length - 1);
            }
          }
          if (generateLatex) {
            if (DEBUG) {
              console.log("allReturnedLatexStrings: " + allReturnedLatexStrings);
            }
            stringToBeReturned = [allReturnedPlainStrings, allReturnedLatexStrings];
          } else {
            stringToBeReturned = allReturnedPlainStrings;
          }
          if (TIMING_DEBUGS) {
            timingDebugWrite = "run time on: " + stringToBeRun + " : " + ((/* @__PURE__ */ new Date()).getTime() - timeStart) + "ms";
            console.log(timingDebugWrite);
          }
          allReturnedPlainStrings = "";
          allReturnedLatexStrings = "";
          return stringToBeReturned;
        };
        check_stack = function() {
          if (tos !== 0) {
            debugger;
            stop("stack error");
          }
          if (frame !== TOS) {
            debugger;
            stop("frame error");
          }
          if (chainOfUserSymbolsNotFunctionsBeingEvaluated.length !== 0) {
            debugger;
            stop("symbols evaluation still ongoing?");
          }
          if (evaluatingAsFloats !== 0) {
            debugger;
            stop("numeric evaluation still ongoing?");
          }
          if (evaluatingPolar !== 0) {
            debugger;
            return stop("evaluation of polar still ongoing?");
          }
        };
        top_level_eval = function() {
          var evalledArgument, originalArgument, shouldAutoexpand;
          if (DEBUG) {
            console.log("#### top level eval");
          }
          trigmode = 0;
          shouldAutoexpand = symbol(AUTOEXPAND);
          if (isZeroAtomOrTensor(get_binding(shouldAutoexpand))) {
            expanding = 0;
          } else {
            expanding = 1;
          }
          originalArgument = top();
          Eval();
          evalledArgument = top();
          if (evalledArgument === symbol(NIL)) {
            return;
          }
          set_binding(symbol(LAST), evalledArgument);
          if (!isZeroAtomOrTensor(get_binding(symbol(BAKE)))) {
            bake();
            evalledArgument = top();
          }
          if ((originalArgument === symbol(SYMBOL_I) || originalArgument === symbol(SYMBOL_J)) && isimaginaryunit(evalledArgument)) {
          } else if (isimaginaryunit(get_binding(symbol(SYMBOL_J)))) {
            push(imaginaryunit);
            push_symbol(SYMBOL_J);
            return subst();
          } else if (isimaginaryunit(get_binding(symbol(SYMBOL_I)))) {
            push(imaginaryunit);
            push_symbol(SYMBOL_I);
            return subst();
          }
        };
        check_esc_flag = function() {
          if (esc_flag) {
            return stop("esc key");
          }
        };
        clearAlgebraEnvironment = function() {
          return do_clearall();
        };
        computeDependenciesFromAlgebra = function(codeFromAlgebraBlock) {
          var i5, keepState, l1, len, len1, m1, originalcodeFromAlgebraBlock, userSimplificationsInProgramForm;
          if (DEBUG) {
            console.log("computeDependenciesFromAlgebra!!!");
          }
          originalcodeFromAlgebraBlock = codeFromAlgebraBlock;
          keepState = true;
          called_from_Algebra_block = true;
          codeFromAlgebraBlock = normaliseDots(codeFromAlgebraBlock);
          if (!keepState) {
            userSimplificationsInListForm = [];
            userSimplificationsInProgramForm = "";
            for (l1 = 0, len = userSimplificationsInListForm.length; l1 < len; l1++) {
              i5 = userSimplificationsInListForm[l1];
              userSimplificationsInProgramForm += "silentpattern(" + car(i5) + "," + car(cdr(i5)) + "," + car(cdr(cdr(i5))) + ")\n";
            }
            do_clearall();
            codeFromAlgebraBlock = userSimplificationsInProgramForm + codeFromAlgebraBlock;
            if (DEBUG) {
              console.log("codeFromAlgebraBlock including patterns: " + codeFromAlgebraBlock);
            }
          }
          if (DEBUG) {
            console.log("computeDependenciesFromAlgebra: patterns in the list --------------- ");
            for (m1 = 0, len1 = userSimplificationsInListForm.length; m1 < len1; m1++) {
              i5 = userSimplificationsInListForm[m1];
              console.log(car(i5) + "," + cdr(i5) + ")");
            }
            console.log("...end of list --------------- ");
          }
          called_from_Algebra_block = false;
          return findDependenciesInScript(codeFromAlgebraBlock, true)[6];
        };
        computeResultsAndJavaScriptFromAlgebra = function(codeFromAlgebraBlock) {
          var code3, dependencyInfo, i5, keepState, l1, latexResult, len, len1, m1, originalcodeFromAlgebraBlock, readableSummaryOfCode, result, stringToBeRun, testableStringIsIgnoredHere, timeStartFromAlgebra, userSimplificationsInProgramForm;
          originalcodeFromAlgebraBlock = codeFromAlgebraBlock;
          keepState = true;
          called_from_Algebra_block = true;
          timeStartFromAlgebra = (/* @__PURE__ */ new Date()).getTime();
          if (TIMING_DEBUGS) {
            console.log(" --------- computeResultsAndJavaScriptFromAlgebra input: " + codeFromAlgebraBlock + " at: " + /* @__PURE__ */ new Date());
          }
          codeFromAlgebraBlock = normaliseDots(codeFromAlgebraBlock);
          stringToBeRun = codeFromAlgebraBlock;
          if (DEBUG) {
            console.log("computeResultsAndJavaScriptFromAlgebra: patterns in the list --------------- ");
            for (l1 = 0, len = userSimplificationsInListForm.length; l1 < len; l1++) {
              i5 = userSimplificationsInListForm[l1];
              console.log(car(i5) + "," + cdr(i5) + ")");
            }
            console.log("...end of list --------------- ");
          }
          if (!keepState) {
            userSimplificationsInListForm = [];
            userSimplificationsInProgramForm = "";
            for (m1 = 0, len1 = userSimplificationsInListForm.length; m1 < len1; m1++) {
              i5 = userSimplificationsInListForm[m1];
              userSimplificationsInProgramForm += "silentpattern(" + car(i5) + "," + car(cdr(i5)) + "," + car(cdr(cdr(i5))) + ")\n";
            }
            do_clearall();
            codeFromAlgebraBlock = userSimplificationsInProgramForm + codeFromAlgebraBlock;
            if (DEBUG) {
              console.log("codeFromAlgebraBlock including patterns: " + codeFromAlgebraBlock);
            }
          }
          [testableStringIsIgnoredHere, result, code3, readableSummaryOfCode, latexResult, errorMessage, dependencyInfo] = findDependenciesInScript(codeFromAlgebraBlock);
          called_from_Algebra_block = false;
          if (readableSummaryOfCode !== "" || errorMessage !== "") {
            result += "\n" + readableSummaryOfCode;
            if (errorMessage !== "") {
              result += "\n" + errorMessage;
            }
            result = result.replace(/\n/g, "\n\n");
            latexResult += "\n$$" + readableSummaryOfCode + "$$";
            if (errorMessage !== "") {
              latexResult += turnErrorMessageToLatex(errorMessage);
            }
            latexResult = latexResult.replace(/\n/g, "\n\n");
          }
          latexResult = latexResult.replace(/\n*/, "");
          latexResult = latexResult.replace(/\$\$\$\$\n*/g, "");
          code3 = code3.replace(/Math\./g, "");
          code3 = code3.replace(/\n/g, "\n\n");
          if (TIMING_DEBUGS) {
            console.log("computeResultsAndJavaScriptFromAlgebra time (total time from notebook and back) for: " + stringToBeRun + " : " + ((/* @__PURE__ */ new Date()).getTime() - timeStartFromAlgebra) + "ms");
          }
          return {
            //code: "// no code generated yet\n//try again later"
            //code: "console.log('some passed code is run'); window.something = 1;"
            code: code3,
            // TODO temporarily pass latex in place of standard result too
            result: latexResult,
            latexResult,
            dependencyInfo
          };
        };
        (typeof exports !== "undefined" && exports !== null ? exports : this).run = run;
        (typeof exports !== "undefined" && exports !== null ? exports : this).findDependenciesInScript = findDependenciesInScript;
        (typeof exports !== "undefined" && exports !== null ? exports : this).computeDependenciesFromAlgebra = computeDependenciesFromAlgebra;
        (typeof exports !== "undefined" && exports !== null ? exports : this).computeResultsAndJavaScriptFromAlgebra = computeResultsAndJavaScriptFromAlgebra;
        (typeof exports !== "undefined" && exports !== null ? exports : this).clearAlgebraEnvironment = clearAlgebraEnvironment;
        tos = 0;
        nil_symbols = 0;
        push = function(p11) {
          if (p11 == null) {
            debugger;
          }
          if (p11.isZero != null) {
            debugger;
          }
          if (p11 === symbol(NIL)) {
            nil_symbols++;
            if (DEBUG) {
              console.log("pushing symbol(NIL) #" + nil_symbols);
            }
          }
          if (tos >= frame) {
            stop("stack overflow");
          }
          return stack[tos++] = p11;
        };
        moveTos = function(stackPos) {
          if (tos <= stackPos) {
            tos = stackPos;
            return;
          }
          while (tos > stackPos) {
            stack[tos] = null;
            tos--;
          }
        };
        top = function() {
          return stack[tos - 1];
        };
        pop = function() {
          var elementToBeReturned;
          if (tos === 0) {
            debugger;
            stop("stack underflow");
          }
          if (stack[tos - 1] == null) {
            debugger;
          }
          elementToBeReturned = stack[--tos];
          stack[tos] = null;
          return elementToBeReturned;
        };
        push_frame = function(n9) {
          var i5, l1, ref2, results;
          i5 = 0;
          frame -= n9;
          if (frame < tos) {
            debugger;
            stop("frame overflow, circular reference?");
          }
          results = [];
          for (i5 = l1 = 0, ref2 = n9; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
            results.push(stack[frame + i5] = symbol(NIL));
          }
          return results;
        };
        pop_frame = function(n9) {
          frame += n9;
          if (frame > TOS) {
            return stop("frame underflow");
          }
        };
        save = function() {
          frame -= 10;
          if (frame < tos) {
            debugger;
            stop("frame overflow, circular reference?");
          }
          stack[frame + 0] = p0;
          stack[frame + 1] = p1;
          stack[frame + 2] = p2;
          stack[frame + 3] = p3;
          stack[frame + 4] = p4;
          stack[frame + 5] = p5;
          stack[frame + 6] = p6;
          stack[frame + 7] = p7;
          stack[frame + 8] = p8;
          return stack[frame + 9] = p9;
        };
        restore = function() {
          if (frame > TOS - 10) {
            stop("frame underflow");
          }
          p0 = stack[frame + 0];
          p1 = stack[frame + 1];
          p2 = stack[frame + 2];
          p3 = stack[frame + 3];
          p4 = stack[frame + 4];
          p5 = stack[frame + 5];
          p6 = stack[frame + 6];
          p7 = stack[frame + 7];
          p8 = stack[frame + 8];
          p9 = stack[frame + 9];
          return frame += 10;
        };
        swap = function() {
          var p11, q;
          p11 = pop();
          q = pop();
          push(p11);
          return push(q);
        };
        dupl = function() {
          var p11;
          p11 = pop();
          push(p11);
          return push(p11);
        };
        $.dupl = dupl;
        $.swap = swap;
        $.restore = restore;
        $.save = save;
        $.push = push;
        $.pop = pop;
        Eval_symbolsinfo = function() {
          var symbolsinfoToBePrinted;
          symbolsinfoToBePrinted = symbolsinfo();
          if (symbolsinfoToBePrinted !== "") {
            return push(new_string(symbolsinfoToBePrinted));
          } else {
            return push_symbol(NIL);
          }
        };
        symbolsinfo = function() {
          var bindingi, i5, l1, ref2, ref3, symbolsinfoToBePrinted, symtabi;
          symbolsinfoToBePrinted = "";
          for (i5 = l1 = ref2 = NIL + 1, ref3 = symtab.length; ref2 <= ref3 ? l1 < ref3 : l1 > ref3; i5 = ref2 <= ref3 ? ++l1 : --l1) {
            if (symtab[i5].printname === "") {
              if (isSymbolReclaimable[i5] === false) {
                break;
              } else {
                continue;
              }
            }
            symtabi = symtab[i5] + "";
            bindingi = (binding[i5] + "").substring(0, 4);
            symbolsinfoToBePrinted += "symbol: " + symtabi + " size: " + countsize(binding[i5]) + " value: " + bindingi + "...\n";
          }
          return symbolsinfoToBePrinted;
        };
        std_symbol = function(s7, n9, latexPrint) {
          var p11;
          p11 = symtab[n9];
          if (p11 == null) {
            debugger;
          }
          p11.printname = s7;
          if (latexPrint != null) {
            return p11.latexPrint = latexPrint;
          } else {
            return p11.latexPrint = s7;
          }
        };
        usr_symbol = function(s7) {
          var i5, l1, ref2;
          i5 = 0;
          for (i5 = l1 = 0, ref2 = NSYM; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
            if (s7 === symtab[i5].printname) {
              return symtab[i5];
            }
            if (symtab[i5].printname === "") {
              break;
            }
          }
          if (i5 === NSYM) {
            stop("symbol table overflow");
          }
          symtab[i5] = new U();
          symtab[i5].k = SYM;
          symtab[i5].printname = s7;
          binding[i5] = symtab[i5];
          isSymbolReclaimable[i5] = false;
          return symtab[i5];
        };
        get_printname = function(p11) {
          if (p11.k !== SYM) {
            stop("symbol error");
          }
          return p11.printname;
        };
        set_binding = function(p11, q) {
          var indexFound;
          if (p11.k !== SYM) {
            stop("symbol error");
          }
          indexFound = symtab.indexOf(p11);
          if (symtab.indexOf(p11, indexFound + 1) !== -1) {
            console.log("ops, more than one element!");
            debugger;
          }
          if (DEBUG) {
            console.log("lookup >> set_binding lookup " + indexFound);
          }
          isSymbolReclaimable[indexFound] = false;
          return binding[indexFound] = q;
        };
        get_binding = function(p11) {
          var indexFound;
          if (p11.k !== SYM) {
            stop("symbol error");
          }
          indexFound = symtab.indexOf(p11);
          if (symtab.indexOf(p11, indexFound + 1) !== -1) {
            console.log("ops, more than one element!");
            debugger;
          }
          if (DEBUG) {
            console.log("lookup >> get_binding lookup " + indexFound);
          }
          return binding[indexFound];
        };
        is_usr_symbol = function(p11) {
          var theSymnum;
          if (p11.k !== SYM) {
            return false;
          }
          theSymnum = symnum(p11);
          if (theSymnum > PI && theSymnum !== SYMBOL_I && theSymnum !== SYMBOL_IDENTITY_MATRIX) {
            return true;
          }
          return false;
        };
        lookupsTotal = 0;
        symnum = function(p11) {
          var indexFound;
          lookupsTotal++;
          if (p11.k !== SYM) {
            stop("symbol error");
          }
          indexFound = symtab.indexOf(p11);
          if (symtab.indexOf(p11, indexFound + 1) !== -1) {
            console.log("ops, more than one element!");
            debugger;
          }
          if (DEBUG) {
            console.log("lookup >> symnum lookup " + indexFound + " lookup # " + lookupsTotal);
          }
          return indexFound;
        };
        push_symbol = function(k2) {
          return push(symtab[k2]);
        };
        clear_symbols = function() {
          var i5, l1, ref2, ref3, results;
          results = [];
          for (i5 = l1 = ref2 = NIL + 1, ref3 = NSYM; ref2 <= ref3 ? l1 < ref3 : l1 > ref3; i5 = ref2 <= ref3 ? ++l1 : --l1) {
            if (symtab[i5].printname === "") {
              if (isSymbolReclaimable[i5] === false) {
                break;
              } else {
                continue;
              }
            }
            symtab[i5] = new U();
            symtab[i5].k = SYM;
            binding[i5] = symtab[i5];
            results.push(isSymbolReclaimable[i5] = false);
          }
          return results;
        };
        collectUserSymbols = function(p11, accumulator = []) {
          var i5, l1, ref2;
          if (is_usr_symbol(p11)) {
            if (accumulator.indexOf(p11) === -1) {
              accumulator.push(p11);
              return;
            }
          }
          if (istensor(p11)) {
            for (i5 = l1 = 0, ref2 = p11.tensor.nelem; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
              collectUserSymbols(p11.tensor.elem[i5], accumulator);
            }
            return;
          }
          while (iscons(p11)) {
            collectUserSymbols(car(p11), accumulator);
            p11 = cdr(p11);
          }
        };
        $.get_binding = get_binding;
        $.set_binding = set_binding;
        $.usr_symbol = usr_symbol;
        $.symbolsinfo = symbolsinfo;
        $.collectUserSymbols = collectUserSymbols;
        if (!inited) {
          inited = true;
          init();
        }
        $.init = init;
        parse_internal = function(argu) {
          if (typeof argu === "string") {
            return scan(argu);
          } else if (typeof argu === "number") {
            if (argu % 1 === 0) {
              return push_integer(argu);
            } else {
              return push_double(argu);
            }
          } else if (argu instanceof U) {
            return push(argu);
          } else {
            console.warn("unknown argument type", argu);
            return push(symbol(NIL));
          }
        };
        parse = function(argu) {
          var data, error;
          try {
            parse_internal(argu);
            data = pop();
            check_stack();
          } catch (error1) {
            error = error1;
            reset_after_error();
            throw error;
          }
          return data;
        };
        exec = function(name, ...argus) {
          var argu, error, fn, l1, len, result;
          fn = get_binding(usr_symbol(name));
          check_stack();
          push(fn);
          for (l1 = 0, len = argus.length; l1 < len; l1++) {
            argu = argus[l1];
            parse_internal(argu);
          }
          list(1 + argus.length);
          p1 = pop();
          push(p1);
          try {
            top_level_eval();
            result = pop();
            check_stack();
          } catch (error1) {
            error = error1;
            reset_after_error();
            throw error;
          }
          return result;
        };
        $.exec = exec;
        $.parse = parse;
        (function() {
          var builtin_fns, fn, l1, len, results;
          builtin_fns = ["abs", "add", "adj", "and", "approxratio", "arccos", "arccosh", "arcsin", "arcsinh", "arctan", "arctanh", "arg", "atomize", "besselj", "bessely", "binding", "binomial", "ceiling", "check", "choose", "circexp", "clear", "clearall", "clearpatterns", "clock", "coeff", "cofactor", "condense", "conj", "contract", "cos", "cosh", "decomp", "defint", "deg", "denominator", "det", "derivative", "dim", "dirac", "divisors", "do", "dot", "draw", "dsolve", "eigen", "eigenval", "eigenvec", "erf", "erfc", "eval", "exp", "expand", "expcos", "expsin", "factor", "factorial", "factorpoly", "filter", "float", "floor", "for", "Gamma", "gcd", "hermite", "hilbert", "imag", "component", "inner", "integral", "inv", "invg", "isinteger", "isprime", "laguerre", "lcm", "leading", "legendre", "log", "mod", "multiply", "not", "nroots", "number", "numerator", "operator", "or", "outer", "pattern", "patternsinfo", "polar", "power", "prime", "print", "print2dascii", "printcomputer", "printlatex", "printlist", "printhuman", "product", "quote", "quotient", "rank", "rationalize", "real", "rect", "roots", "round", "equals", "shape", "sgn", "silentpattern", "simplify", "sin", "sinh", "sqrt", "stop", "subst", "sum", "symbolsinfo", "tan", "tanh", "taylor", "test", "testeq", "testge", "testgt", "testle", "testlt", "transpose", "unit", "zero"];
          results = [];
          for (l1 = 0, len = builtin_fns.length; l1 < len; l1++) {
            fn = builtin_fns[l1];
            results.push($[fn] = exec.bind(this, fn));
          }
          return results;
        })();
        freeze = function() {
          var frozenContents, frozenHash, frozenPatterns, frozenSymbols, i5, l1, ref2;
          frozenSymbols = [];
          frozenContents = [];
          frozenPatterns = [];
          frozenHash = "";
          for (i5 = l1 = 0, ref2 = symtab.length; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
            if (isSymbolReclaimable[i5] === false) {
              frozenSymbols.push(symtab[i5]);
              frozenContents.push(binding[i5]);
            }
          }
          frozenPatterns = userSimplificationsInListForm.slice(0);
          return [frozenSymbols, frozenContents, frozenPatterns, zero, one, imaginaryunit, getStateHash()];
        };
        unfreeze = function(frozen) {
          var frozenContents, frozenPatterns, frozenSymbols, i5, l1, ref2;
          [frozenSymbols, frozenContents, frozenPatterns, zero, one, imaginaryunit] = frozen;
          for (i5 = l1 = 0, ref2 = frozenSymbols.length; 0 <= ref2 ? l1 < ref2 : l1 > ref2; i5 = 0 <= ref2 ? ++l1 : --l1) {
            symtab[i5] = frozenSymbols[i5];
            binding[i5] = frozenContents[i5];
          }
          return userSimplificationsInListForm = frozenPatterns.slice(0);
        };
        compareState = function(previousHash) {
          var frozenHash;
          frozenHash = getStateHash();
          if (frozenHash === previousHash) {
            return true;
          } else {
            return false;
          }
        };
        getStateHash = function() {
          var bindingi, frozenHash, i5, l1, len, m1, ref2, ref3, symtabi;
          frozenHash = "";
          for (i5 = l1 = ref2 = NIL + 1, ref3 = symtab.length; ref2 <= ref3 ? l1 < ref3 : l1 > ref3; i5 = ref2 <= ref3 ? ++l1 : --l1) {
            if (symtab[i5].printname === "") {
              if (isSymbolReclaimable[i5] === false) {
                break;
              } else {
                continue;
              }
            }
            symtabi = print_list(symtab[i5]);
            bindingi = print_list(binding[i5]);
            frozenHash += " //" + symtabi + " : " + bindingi;
          }
          for (m1 = 0, len = userSimplificationsInListForm.length; m1 < len; m1++) {
            i5 = userSimplificationsInListForm[m1];
            frozenHash += " pattern: " + i5;
          }
          if (DEBUG) {
            console.log("frozenHash: " + frozenHash);
          }
          return frozenHash;
        };
      }).call(exports);
    }
  });

  // ../../../../../../../../Dropbox/projects/hazel/node_modules/@lit/reactive-element/css-tag.js
  var t = window;
  var e = t.ShadowRoot && (void 0 === t.ShadyCSS || t.ShadyCSS.nativeShadow) && "adoptedStyleSheets" in Document.prototype && "replace" in CSSStyleSheet.prototype;
  var s = Symbol();
  var n = /* @__PURE__ */ new WeakMap();
  var o2 = class {
    constructor(t5, e11, n9) {
      if (this._$cssResult$ = true, n9 !== s) throw Error("CSSResult is not constructable. Use `unsafeCSS` or `css` instead.");
      this.cssText = t5, this.t = e11;
    }
    get styleSheet() {
      let t5 = this.o;
      const s7 = this.t;
      if (e && void 0 === t5) {
        const e11 = void 0 !== s7 && 1 === s7.length;
        e11 && (t5 = n.get(s7)), void 0 === t5 && ((this.o = t5 = new CSSStyleSheet()).replaceSync(this.cssText), e11 && n.set(s7, t5));
      }
      return t5;
    }
    toString() {
      return this.cssText;
    }
  };
  var r = (t5) => new o2("string" == typeof t5 ? t5 : t5 + "", void 0, s);
  var i = (t5, ...e11) => {
    const n9 = 1 === t5.length ? t5[0] : e11.reduce((e12, s7, n10) => e12 + ((t6) => {
      if (true === t6._$cssResult$) return t6.cssText;
      if ("number" == typeof t6) return t6;
      throw Error("Value passed to 'css' function must be a 'css' function result: " + t6 + ". Use 'unsafeCSS' to pass non-literal values, but take care to ensure page security.");
    })(s7) + t5[n10 + 1], t5[0]);
    return new o2(n9, t5, s);
  };
  var S = (s7, n9) => {
    e ? s7.adoptedStyleSheets = n9.map((t5) => t5 instanceof CSSStyleSheet ? t5 : t5.styleSheet) : n9.forEach((e11) => {
      const n10 = document.createElement("style"), o12 = t.litNonce;
      void 0 !== o12 && n10.setAttribute("nonce", o12), n10.textContent = e11.cssText, s7.appendChild(n10);
    });
  };
  var c = e ? (t5) => t5 : (t5) => t5 instanceof CSSStyleSheet ? ((t6) => {
    let e11 = "";
    for (const s7 of t6.cssRules) e11 += s7.cssText;
    return r(e11);
  })(t5) : t5;

  // ../../../../../../../../Dropbox/projects/hazel/node_modules/@lit/reactive-element/reactive-element.js
  var s2;
  var e2 = window;
  var r2 = e2.trustedTypes;
  var h = r2 ? r2.emptyScript : "";
  var o3 = e2.reactiveElementPolyfillSupport;
  var n2 = { toAttribute(t5, i5) {
    switch (i5) {
      case Boolean:
        t5 = t5 ? h : null;
        break;
      case Object:
      case Array:
        t5 = null == t5 ? t5 : JSON.stringify(t5);
    }
    return t5;
  }, fromAttribute(t5, i5) {
    let s7 = t5;
    switch (i5) {
      case Boolean:
        s7 = null !== t5;
        break;
      case Number:
        s7 = null === t5 ? null : Number(t5);
        break;
      case Object:
      case Array:
        try {
          s7 = JSON.parse(t5);
        } catch (t6) {
          s7 = null;
        }
    }
    return s7;
  } };
  var a = (t5, i5) => i5 !== t5 && (i5 == i5 || t5 == t5);
  var l = { attribute: true, type: String, converter: n2, reflect: false, hasChanged: a };
  var d = "finalized";
  var u = class extends HTMLElement {
    constructor() {
      super(), this._$Ei = /* @__PURE__ */ new Map(), this.isUpdatePending = false, this.hasUpdated = false, this._$El = null, this._$Eu();
    }
    static addInitializer(t5) {
      var i5;
      this.finalize(), (null !== (i5 = this.h) && void 0 !== i5 ? i5 : this.h = []).push(t5);
    }
    static get observedAttributes() {
      this.finalize();
      const t5 = [];
      return this.elementProperties.forEach((i5, s7) => {
        const e11 = this._$Ep(s7, i5);
        void 0 !== e11 && (this._$Ev.set(e11, s7), t5.push(e11));
      }), t5;
    }
    static createProperty(t5, i5 = l) {
      if (i5.state && (i5.attribute = false), this.finalize(), this.elementProperties.set(t5, i5), !i5.noAccessor && !this.prototype.hasOwnProperty(t5)) {
        const s7 = "symbol" == typeof t5 ? Symbol() : "__" + t5, e11 = this.getPropertyDescriptor(t5, s7, i5);
        void 0 !== e11 && Object.defineProperty(this.prototype, t5, e11);
      }
    }
    static getPropertyDescriptor(t5, i5, s7) {
      return { get() {
        return this[i5];
      }, set(e11) {
        const r6 = this[t5];
        this[i5] = e11, this.requestUpdate(t5, r6, s7);
      }, configurable: true, enumerable: true };
    }
    static getPropertyOptions(t5) {
      return this.elementProperties.get(t5) || l;
    }
    static finalize() {
      if (this.hasOwnProperty(d)) return false;
      this[d] = true;
      const t5 = Object.getPrototypeOf(this);
      if (t5.finalize(), void 0 !== t5.h && (this.h = [...t5.h]), this.elementProperties = new Map(t5.elementProperties), this._$Ev = /* @__PURE__ */ new Map(), this.hasOwnProperty("properties")) {
        const t6 = this.properties, i5 = [...Object.getOwnPropertyNames(t6), ...Object.getOwnPropertySymbols(t6)];
        for (const s7 of i5) this.createProperty(s7, t6[s7]);
      }
      return this.elementStyles = this.finalizeStyles(this.styles), true;
    }
    static finalizeStyles(i5) {
      const s7 = [];
      if (Array.isArray(i5)) {
        const e11 = new Set(i5.flat(1 / 0).reverse());
        for (const i6 of e11) s7.unshift(c(i6));
      } else void 0 !== i5 && s7.push(c(i5));
      return s7;
    }
    static _$Ep(t5, i5) {
      const s7 = i5.attribute;
      return false === s7 ? void 0 : "string" == typeof s7 ? s7 : "string" == typeof t5 ? t5.toLowerCase() : void 0;
    }
    _$Eu() {
      var t5;
      this._$E_ = new Promise((t6) => this.enableUpdating = t6), this._$AL = /* @__PURE__ */ new Map(), this._$Eg(), this.requestUpdate(), null === (t5 = this.constructor.h) || void 0 === t5 || t5.forEach((t6) => t6(this));
    }
    addController(t5) {
      var i5, s7;
      (null !== (i5 = this._$ES) && void 0 !== i5 ? i5 : this._$ES = []).push(t5), void 0 !== this.renderRoot && this.isConnected && (null === (s7 = t5.hostConnected) || void 0 === s7 || s7.call(t5));
    }
    removeController(t5) {
      var i5;
      null === (i5 = this._$ES) || void 0 === i5 || i5.splice(this._$ES.indexOf(t5) >>> 0, 1);
    }
    _$Eg() {
      this.constructor.elementProperties.forEach((t5, i5) => {
        this.hasOwnProperty(i5) && (this._$Ei.set(i5, this[i5]), delete this[i5]);
      });
    }
    createRenderRoot() {
      var t5;
      const s7 = null !== (t5 = this.shadowRoot) && void 0 !== t5 ? t5 : this.attachShadow(this.constructor.shadowRootOptions);
      return S(s7, this.constructor.elementStyles), s7;
    }
    connectedCallback() {
      var t5;
      void 0 === this.renderRoot && (this.renderRoot = this.createRenderRoot()), this.enableUpdating(true), null === (t5 = this._$ES) || void 0 === t5 || t5.forEach((t6) => {
        var i5;
        return null === (i5 = t6.hostConnected) || void 0 === i5 ? void 0 : i5.call(t6);
      });
    }
    enableUpdating(t5) {
    }
    disconnectedCallback() {
      var t5;
      null === (t5 = this._$ES) || void 0 === t5 || t5.forEach((t6) => {
        var i5;
        return null === (i5 = t6.hostDisconnected) || void 0 === i5 ? void 0 : i5.call(t6);
      });
    }
    attributeChangedCallback(t5, i5, s7) {
      this._$AK(t5, s7);
    }
    _$EO(t5, i5, s7 = l) {
      var e11;
      const r6 = this.constructor._$Ep(t5, s7);
      if (void 0 !== r6 && true === s7.reflect) {
        const h5 = (void 0 !== (null === (e11 = s7.converter) || void 0 === e11 ? void 0 : e11.toAttribute) ? s7.converter : n2).toAttribute(i5, s7.type);
        this._$El = t5, null == h5 ? this.removeAttribute(r6) : this.setAttribute(r6, h5), this._$El = null;
      }
    }
    _$AK(t5, i5) {
      var s7;
      const e11 = this.constructor, r6 = e11._$Ev.get(t5);
      if (void 0 !== r6 && this._$El !== r6) {
        const t6 = e11.getPropertyOptions(r6), h5 = "function" == typeof t6.converter ? { fromAttribute: t6.converter } : void 0 !== (null === (s7 = t6.converter) || void 0 === s7 ? void 0 : s7.fromAttribute) ? t6.converter : n2;
        this._$El = r6, this[r6] = h5.fromAttribute(i5, t6.type), this._$El = null;
      }
    }
    requestUpdate(t5, i5, s7) {
      let e11 = true;
      void 0 !== t5 && (((s7 = s7 || this.constructor.getPropertyOptions(t5)).hasChanged || a)(this[t5], i5) ? (this._$AL.has(t5) || this._$AL.set(t5, i5), true === s7.reflect && this._$El !== t5 && (void 0 === this._$EC && (this._$EC = /* @__PURE__ */ new Map()), this._$EC.set(t5, s7))) : e11 = false), !this.isUpdatePending && e11 && (this._$E_ = this._$Ej());
    }
    async _$Ej() {
      this.isUpdatePending = true;
      try {
        await this._$E_;
      } catch (t6) {
        Promise.reject(t6);
      }
      const t5 = this.scheduleUpdate();
      return null != t5 && await t5, !this.isUpdatePending;
    }
    scheduleUpdate() {
      return this.performUpdate();
    }
    performUpdate() {
      var t5;
      if (!this.isUpdatePending) return;
      this.hasUpdated, this._$Ei && (this._$Ei.forEach((t6, i6) => this[i6] = t6), this._$Ei = void 0);
      let i5 = false;
      const s7 = this._$AL;
      try {
        i5 = this.shouldUpdate(s7), i5 ? (this.willUpdate(s7), null === (t5 = this._$ES) || void 0 === t5 || t5.forEach((t6) => {
          var i6;
          return null === (i6 = t6.hostUpdate) || void 0 === i6 ? void 0 : i6.call(t6);
        }), this.update(s7)) : this._$Ek();
      } catch (t6) {
        throw i5 = false, this._$Ek(), t6;
      }
      i5 && this._$AE(s7);
    }
    willUpdate(t5) {
    }
    _$AE(t5) {
      var i5;
      null === (i5 = this._$ES) || void 0 === i5 || i5.forEach((t6) => {
        var i6;
        return null === (i6 = t6.hostUpdated) || void 0 === i6 ? void 0 : i6.call(t6);
      }), this.hasUpdated || (this.hasUpdated = true, this.firstUpdated(t5)), this.updated(t5);
    }
    _$Ek() {
      this._$AL = /* @__PURE__ */ new Map(), this.isUpdatePending = false;
    }
    get updateComplete() {
      return this.getUpdateComplete();
    }
    getUpdateComplete() {
      return this._$E_;
    }
    shouldUpdate(t5) {
      return true;
    }
    update(t5) {
      void 0 !== this._$EC && (this._$EC.forEach((t6, i5) => this._$EO(i5, this[i5], t6)), this._$EC = void 0), this._$Ek();
    }
    updated(t5) {
    }
    firstUpdated(t5) {
    }
  };
  u[d] = true, u.elementProperties = /* @__PURE__ */ new Map(), u.elementStyles = [], u.shadowRootOptions = { mode: "open" }, null == o3 || o3({ ReactiveElement: u }), (null !== (s2 = e2.reactiveElementVersions) && void 0 !== s2 ? s2 : e2.reactiveElementVersions = []).push("1.6.3");

  // ../../../../../../../../Dropbox/projects/hazel/node_modules/lit-html/lit-html.js
  var t2;
  var i2 = window;
  var s3 = i2.trustedTypes;
  var e3 = s3 ? s3.createPolicy("lit-html", { createHTML: (t5) => t5 }) : void 0;
  var o4 = "$lit$";
  var n3 = `lit$${(Math.random() + "").slice(9)}$`;
  var l2 = "?" + n3;
  var h2 = `<${l2}>`;
  var r3 = document;
  var u2 = () => r3.createComment("");
  var d2 = (t5) => null === t5 || "object" != typeof t5 && "function" != typeof t5;
  var c2 = Array.isArray;
  var v = (t5) => c2(t5) || "function" == typeof (null == t5 ? void 0 : t5[Symbol.iterator]);
  var a2 = "[ 	\n\f\r]";
  var f = /<(?:(!--|\/[^a-zA-Z])|(\/?[a-zA-Z][^>\s]*)|(\/?$))/g;
  var _ = /-->/g;
  var m = />/g;
  var p = RegExp(`>|${a2}(?:([^\\s"'>=/]+)(${a2}*=${a2}*(?:[^ 	
\f\r"'\`<>=]|("|')|))|$)`, "g");
  var g = /'/g;
  var $2 = /"/g;
  var y = /^(?:script|style|textarea|title)$/i;
  var w = (t5) => (i5, ...s7) => ({ _$litType$: t5, strings: i5, values: s7 });
  var x = w(1);
  var b = w(2);
  var T = Symbol.for("lit-noChange");
  var A = Symbol.for("lit-nothing");
  var E2 = /* @__PURE__ */ new WeakMap();
  var C = r3.createTreeWalker(r3, 129, null, false);
  function P(t5, i5) {
    if (!Array.isArray(t5) || !t5.hasOwnProperty("raw")) throw Error("invalid template strings array");
    return void 0 !== e3 ? e3.createHTML(i5) : i5;
  }
  var V = (t5, i5) => {
    const s7 = t5.length - 1, e11 = [];
    let l8, r6 = 2 === i5 ? "<svg>" : "", u4 = f;
    for (let i6 = 0; i6 < s7; i6++) {
      const s8 = t5[i6];
      let d3, c6, v2 = -1, a4 = 0;
      for (; a4 < s8.length && (u4.lastIndex = a4, c6 = u4.exec(s8), null !== c6); ) a4 = u4.lastIndex, u4 === f ? "!--" === c6[1] ? u4 = _ : void 0 !== c6[1] ? u4 = m : void 0 !== c6[2] ? (y.test(c6[2]) && (l8 = RegExp("</" + c6[2], "g")), u4 = p) : void 0 !== c6[3] && (u4 = p) : u4 === p ? ">" === c6[0] ? (u4 = null != l8 ? l8 : f, v2 = -1) : void 0 === c6[1] ? v2 = -2 : (v2 = u4.lastIndex - c6[2].length, d3 = c6[1], u4 = void 0 === c6[3] ? p : '"' === c6[3] ? $2 : g) : u4 === $2 || u4 === g ? u4 = p : u4 === _ || u4 === m ? u4 = f : (u4 = p, l8 = void 0);
      const w2 = u4 === p && t5[i6 + 1].startsWith("/>") ? " " : "";
      r6 += u4 === f ? s8 + h2 : v2 >= 0 ? (e11.push(d3), s8.slice(0, v2) + o4 + s8.slice(v2) + n3 + w2) : s8 + n3 + (-2 === v2 ? (e11.push(void 0), i6) : w2);
    }
    return [P(t5, r6 + (t5[s7] || "<?>") + (2 === i5 ? "</svg>" : "")), e11];
  };
  var N2 = class _N {
    constructor({ strings: t5, _$litType$: i5 }, e11) {
      let h5;
      this.parts = [];
      let r6 = 0, d3 = 0;
      const c6 = t5.length - 1, v2 = this.parts, [a4, f7] = V(t5, i5);
      if (this.el = _N.createElement(a4, e11), C.currentNode = this.el.content, 2 === i5) {
        const t6 = this.el.content, i6 = t6.firstChild;
        i6.remove(), t6.append(...i6.childNodes);
      }
      for (; null !== (h5 = C.nextNode()) && v2.length < c6; ) {
        if (1 === h5.nodeType) {
          if (h5.hasAttributes()) {
            const t6 = [];
            for (const i6 of h5.getAttributeNames()) if (i6.endsWith(o4) || i6.startsWith(n3)) {
              const s7 = f7[d3++];
              if (t6.push(i6), void 0 !== s7) {
                const t7 = h5.getAttribute(s7.toLowerCase() + o4).split(n3), i7 = /([.?@])?(.*)/.exec(s7);
                v2.push({ type: 1, index: r6, name: i7[2], strings: t7, ctor: "." === i7[1] ? H : "?" === i7[1] ? L : "@" === i7[1] ? z : k });
              } else v2.push({ type: 6, index: r6 });
            }
            for (const i6 of t6) h5.removeAttribute(i6);
          }
          if (y.test(h5.tagName)) {
            const t6 = h5.textContent.split(n3), i6 = t6.length - 1;
            if (i6 > 0) {
              h5.textContent = s3 ? s3.emptyScript : "";
              for (let s7 = 0; s7 < i6; s7++) h5.append(t6[s7], u2()), C.nextNode(), v2.push({ type: 2, index: ++r6 });
              h5.append(t6[i6], u2());
            }
          }
        } else if (8 === h5.nodeType) if (h5.data === l2) v2.push({ type: 2, index: r6 });
        else {
          let t6 = -1;
          for (; -1 !== (t6 = h5.data.indexOf(n3, t6 + 1)); ) v2.push({ type: 7, index: r6 }), t6 += n3.length - 1;
        }
        r6++;
      }
    }
    static createElement(t5, i5) {
      const s7 = r3.createElement("template");
      return s7.innerHTML = t5, s7;
    }
  };
  function S2(t5, i5, s7 = t5, e11) {
    var o12, n9, l8, h5;
    if (i5 === T) return i5;
    let r6 = void 0 !== e11 ? null === (o12 = s7._$Co) || void 0 === o12 ? void 0 : o12[e11] : s7._$Cl;
    const u4 = d2(i5) ? void 0 : i5._$litDirective$;
    return (null == r6 ? void 0 : r6.constructor) !== u4 && (null === (n9 = null == r6 ? void 0 : r6._$AO) || void 0 === n9 || n9.call(r6, false), void 0 === u4 ? r6 = void 0 : (r6 = new u4(t5), r6._$AT(t5, s7, e11)), void 0 !== e11 ? (null !== (l8 = (h5 = s7)._$Co) && void 0 !== l8 ? l8 : h5._$Co = [])[e11] = r6 : s7._$Cl = r6), void 0 !== r6 && (i5 = S2(t5, r6._$AS(t5, i5.values), r6, e11)), i5;
  }
  var M2 = class {
    constructor(t5, i5) {
      this._$AV = [], this._$AN = void 0, this._$AD = t5, this._$AM = i5;
    }
    get parentNode() {
      return this._$AM.parentNode;
    }
    get _$AU() {
      return this._$AM._$AU;
    }
    u(t5) {
      var i5;
      const { el: { content: s7 }, parts: e11 } = this._$AD, o12 = (null !== (i5 = null == t5 ? void 0 : t5.creationScope) && void 0 !== i5 ? i5 : r3).importNode(s7, true);
      C.currentNode = o12;
      let n9 = C.nextNode(), l8 = 0, h5 = 0, u4 = e11[0];
      for (; void 0 !== u4; ) {
        if (l8 === u4.index) {
          let i6;
          2 === u4.type ? i6 = new R(n9, n9.nextSibling, this, t5) : 1 === u4.type ? i6 = new u4.ctor(n9, u4.name, u4.strings, this, t5) : 6 === u4.type && (i6 = new Z(n9, this, t5)), this._$AV.push(i6), u4 = e11[++h5];
        }
        l8 !== (null == u4 ? void 0 : u4.index) && (n9 = C.nextNode(), l8++);
      }
      return C.currentNode = r3, o12;
    }
    v(t5) {
      let i5 = 0;
      for (const s7 of this._$AV) void 0 !== s7 && (void 0 !== s7.strings ? (s7._$AI(t5, s7, i5), i5 += s7.strings.length - 2) : s7._$AI(t5[i5])), i5++;
    }
  };
  var R = class _R {
    constructor(t5, i5, s7, e11) {
      var o12;
      this.type = 2, this._$AH = A, this._$AN = void 0, this._$AA = t5, this._$AB = i5, this._$AM = s7, this.options = e11, this._$Cp = null === (o12 = null == e11 ? void 0 : e11.isConnected) || void 0 === o12 || o12;
    }
    get _$AU() {
      var t5, i5;
      return null !== (i5 = null === (t5 = this._$AM) || void 0 === t5 ? void 0 : t5._$AU) && void 0 !== i5 ? i5 : this._$Cp;
    }
    get parentNode() {
      let t5 = this._$AA.parentNode;
      const i5 = this._$AM;
      return void 0 !== i5 && 11 === (null == t5 ? void 0 : t5.nodeType) && (t5 = i5.parentNode), t5;
    }
    get startNode() {
      return this._$AA;
    }
    get endNode() {
      return this._$AB;
    }
    _$AI(t5, i5 = this) {
      t5 = S2(this, t5, i5), d2(t5) ? t5 === A || null == t5 || "" === t5 ? (this._$AH !== A && this._$AR(), this._$AH = A) : t5 !== this._$AH && t5 !== T && this._(t5) : void 0 !== t5._$litType$ ? this.g(t5) : void 0 !== t5.nodeType ? this.$(t5) : v(t5) ? this.T(t5) : this._(t5);
    }
    k(t5) {
      return this._$AA.parentNode.insertBefore(t5, this._$AB);
    }
    $(t5) {
      this._$AH !== t5 && (this._$AR(), this._$AH = this.k(t5));
    }
    _(t5) {
      this._$AH !== A && d2(this._$AH) ? this._$AA.nextSibling.data = t5 : this.$(r3.createTextNode(t5)), this._$AH = t5;
    }
    g(t5) {
      var i5;
      const { values: s7, _$litType$: e11 } = t5, o12 = "number" == typeof e11 ? this._$AC(t5) : (void 0 === e11.el && (e11.el = N2.createElement(P(e11.h, e11.h[0]), this.options)), e11);
      if ((null === (i5 = this._$AH) || void 0 === i5 ? void 0 : i5._$AD) === o12) this._$AH.v(s7);
      else {
        const t6 = new M2(o12, this), i6 = t6.u(this.options);
        t6.v(s7), this.$(i6), this._$AH = t6;
      }
    }
    _$AC(t5) {
      let i5 = E2.get(t5.strings);
      return void 0 === i5 && E2.set(t5.strings, i5 = new N2(t5)), i5;
    }
    T(t5) {
      c2(this._$AH) || (this._$AH = [], this._$AR());
      const i5 = this._$AH;
      let s7, e11 = 0;
      for (const o12 of t5) e11 === i5.length ? i5.push(s7 = new _R(this.k(u2()), this.k(u2()), this, this.options)) : s7 = i5[e11], s7._$AI(o12), e11++;
      e11 < i5.length && (this._$AR(s7 && s7._$AB.nextSibling, e11), i5.length = e11);
    }
    _$AR(t5 = this._$AA.nextSibling, i5) {
      var s7;
      for (null === (s7 = this._$AP) || void 0 === s7 || s7.call(this, false, true, i5); t5 && t5 !== this._$AB; ) {
        const i6 = t5.nextSibling;
        t5.remove(), t5 = i6;
      }
    }
    setConnected(t5) {
      var i5;
      void 0 === this._$AM && (this._$Cp = t5, null === (i5 = this._$AP) || void 0 === i5 || i5.call(this, t5));
    }
  };
  var k = class {
    constructor(t5, i5, s7, e11, o12) {
      this.type = 1, this._$AH = A, this._$AN = void 0, this.element = t5, this.name = i5, this._$AM = e11, this.options = o12, s7.length > 2 || "" !== s7[0] || "" !== s7[1] ? (this._$AH = Array(s7.length - 1).fill(new String()), this.strings = s7) : this._$AH = A;
    }
    get tagName() {
      return this.element.tagName;
    }
    get _$AU() {
      return this._$AM._$AU;
    }
    _$AI(t5, i5 = this, s7, e11) {
      const o12 = this.strings;
      let n9 = false;
      if (void 0 === o12) t5 = S2(this, t5, i5, 0), n9 = !d2(t5) || t5 !== this._$AH && t5 !== T, n9 && (this._$AH = t5);
      else {
        const e12 = t5;
        let l8, h5;
        for (t5 = o12[0], l8 = 0; l8 < o12.length - 1; l8++) h5 = S2(this, e12[s7 + l8], i5, l8), h5 === T && (h5 = this._$AH[l8]), n9 || (n9 = !d2(h5) || h5 !== this._$AH[l8]), h5 === A ? t5 = A : t5 !== A && (t5 += (null != h5 ? h5 : "") + o12[l8 + 1]), this._$AH[l8] = h5;
      }
      n9 && !e11 && this.j(t5);
    }
    j(t5) {
      t5 === A ? this.element.removeAttribute(this.name) : this.element.setAttribute(this.name, null != t5 ? t5 : "");
    }
  };
  var H = class extends k {
    constructor() {
      super(...arguments), this.type = 3;
    }
    j(t5) {
      this.element[this.name] = t5 === A ? void 0 : t5;
    }
  };
  var I = s3 ? s3.emptyScript : "";
  var L = class extends k {
    constructor() {
      super(...arguments), this.type = 4;
    }
    j(t5) {
      t5 && t5 !== A ? this.element.setAttribute(this.name, I) : this.element.removeAttribute(this.name);
    }
  };
  var z = class extends k {
    constructor(t5, i5, s7, e11, o12) {
      super(t5, i5, s7, e11, o12), this.type = 5;
    }
    _$AI(t5, i5 = this) {
      var s7;
      if ((t5 = null !== (s7 = S2(this, t5, i5, 0)) && void 0 !== s7 ? s7 : A) === T) return;
      const e11 = this._$AH, o12 = t5 === A && e11 !== A || t5.capture !== e11.capture || t5.once !== e11.once || t5.passive !== e11.passive, n9 = t5 !== A && (e11 === A || o12);
      o12 && this.element.removeEventListener(this.name, this, e11), n9 && this.element.addEventListener(this.name, this, t5), this._$AH = t5;
    }
    handleEvent(t5) {
      var i5, s7;
      "function" == typeof this._$AH ? this._$AH.call(null !== (s7 = null === (i5 = this.options) || void 0 === i5 ? void 0 : i5.host) && void 0 !== s7 ? s7 : this.element, t5) : this._$AH.handleEvent(t5);
    }
  };
  var Z = class {
    constructor(t5, i5, s7) {
      this.element = t5, this.type = 6, this._$AN = void 0, this._$AM = i5, this.options = s7;
    }
    get _$AU() {
      return this._$AM._$AU;
    }
    _$AI(t5) {
      S2(this, t5);
    }
  };
  var j = { O: o4, P: n3, A: l2, C: 1, M: V, L: M2, R: v, D: S2, I: R, V: k, H: L, N: z, U: H, F: Z };
  var B = i2.litHtmlPolyfillSupport;
  null == B || B(N2, R), (null !== (t2 = i2.litHtmlVersions) && void 0 !== t2 ? t2 : i2.litHtmlVersions = []).push("2.8.0");
  var D = (t5, i5, s7) => {
    var e11, o12;
    const n9 = null !== (e11 = null == s7 ? void 0 : s7.renderBefore) && void 0 !== e11 ? e11 : i5;
    let l8 = n9._$litPart$;
    if (void 0 === l8) {
      const t6 = null !== (o12 = null == s7 ? void 0 : s7.renderBefore) && void 0 !== o12 ? o12 : null;
      n9._$litPart$ = l8 = new R(i5.insertBefore(u2(), t6), t6, void 0, null != s7 ? s7 : {});
    }
    return l8._$AI(t5), l8;
  };

  // ../../../../../../../../Dropbox/projects/hazel/node_modules/lit-element/lit-element.js
  var l3;
  var o5;
  var s4 = class extends u {
    constructor() {
      super(...arguments), this.renderOptions = { host: this }, this._$Do = void 0;
    }
    createRenderRoot() {
      var t5, e11;
      const i5 = super.createRenderRoot();
      return null !== (t5 = (e11 = this.renderOptions).renderBefore) && void 0 !== t5 || (e11.renderBefore = i5.firstChild), i5;
    }
    update(t5) {
      const i5 = this.render();
      this.hasUpdated || (this.renderOptions.isConnected = this.isConnected), super.update(t5), this._$Do = D(i5, this.renderRoot, this.renderOptions);
    }
    connectedCallback() {
      var t5;
      super.connectedCallback(), null === (t5 = this._$Do) || void 0 === t5 || t5.setConnected(true);
    }
    disconnectedCallback() {
      var t5;
      super.disconnectedCallback(), null === (t5 = this._$Do) || void 0 === t5 || t5.setConnected(false);
    }
    render() {
      return T;
    }
  };
  s4.finalized = true, s4._$litElement$ = true, null === (l3 = globalThis.litElementHydrateSupport) || void 0 === l3 || l3.call(globalThis, { LitElement: s4 });
  var n4 = globalThis.litElementPolyfillSupport;
  null == n4 || n4({ LitElement: s4 });
  (null !== (o5 = globalThis.litElementVersions) && void 0 !== o5 ? o5 : globalThis.litElementVersions = []).push("3.3.3");

  // ../../../../../../../../Dropbox/projects/hazel/node_modules/@lit/reactive-element/decorators/custom-element.js
  var e4 = (e11) => (n9) => "function" == typeof n9 ? ((e12, n10) => (customElements.define(e12, n10), n10))(e11, n9) : ((e12, n10) => {
    const { kind: t5, elements: s7 } = n10;
    return { kind: t5, elements: s7, finisher(n11) {
      customElements.define(e12, n11);
    } };
  })(e11, n9);

  // ../../../../../../../../Dropbox/projects/hazel/node_modules/@lit/reactive-element/decorators/property.js
  var i3 = (i5, e11) => "method" === e11.kind && e11.descriptor && !("value" in e11.descriptor) ? { ...e11, finisher(n9) {
    n9.createProperty(e11.key, i5);
  } } : { kind: "field", key: Symbol(), placement: "own", descriptor: {}, originalKey: e11.key, initializer() {
    "function" == typeof e11.initializer && (this[e11.key] = e11.initializer.call(this));
  }, finisher(n9) {
    n9.createProperty(e11.key, i5);
  } };
  var e5 = (i5, e11, n9) => {
    e11.constructor.createProperty(n9, i5);
  };
  function n5(n9) {
    return (t5, o12) => void 0 !== o12 ? e5(n9, t5, o12) : i3(n9, t5);
  }

  // ../../../../../../../../Dropbox/projects/hazel/node_modules/@lit/reactive-element/decorators/state.js
  function t3(t5) {
    return n5({ ...t5, state: true });
  }

  // ../../../../../../../../Dropbox/projects/hazel/node_modules/@lit/reactive-element/decorators/query-assigned-elements.js
  var n6;
  var e6 = null != (null === (n6 = window.HTMLSlotElement) || void 0 === n6 ? void 0 : n6.prototype.assignedElements) ? (o12, n9) => o12.assignedElements(n9) : (o12, n9) => o12.assignedNodes(n9).filter((o13) => o13.nodeType === Node.ELEMENT_NODE);

  // ../../../../../../../../Dropbox/projects/hazel/node_modules/lit-html/directive.js
  var t4 = { ATTRIBUTE: 1, CHILD: 2, PROPERTY: 3, BOOLEAN_ATTRIBUTE: 4, EVENT: 5, ELEMENT: 6 };
  var e7 = (t5) => (...e11) => ({ _$litDirective$: t5, values: e11 });
  var i4 = class {
    constructor(t5) {
    }
    get _$AU() {
      return this._$AM._$AU;
    }
    _$AT(t5, e11, i5) {
      this._$Ct = t5, this._$AM = e11, this._$Ci = i5;
    }
    _$AS(t5, e11) {
      return this.update(t5, e11);
    }
    update(t5, e11) {
      return this.render(...e11);
    }
  };

  // ../../../../../../../../Dropbox/projects/hazel/node_modules/lit-html/directive-helpers.js
  var { I: l5 } = j;
  var e8 = (o12) => void 0 === o12.strings;
  var r4 = () => document.createComment("");
  var c3 = (o12, i5, n9) => {
    var t5;
    const v2 = o12._$AA.parentNode, d3 = void 0 === i5 ? o12._$AB : i5._$AA;
    if (void 0 === n9) {
      const i6 = v2.insertBefore(r4(), d3), t6 = v2.insertBefore(r4(), d3);
      n9 = new l5(i6, t6, o12, o12.options);
    } else {
      const l8 = n9._$AB.nextSibling, i6 = n9._$AM, u4 = i6 !== o12;
      if (u4) {
        let l9;
        null === (t5 = n9._$AQ) || void 0 === t5 || t5.call(n9, o12), n9._$AM = o12, void 0 !== n9._$AP && (l9 = o12._$AU) !== i6._$AU && n9._$AP(l9);
      }
      if (l8 !== d3 || u4) {
        let o13 = n9._$AA;
        for (; o13 !== l8; ) {
          const l9 = o13.nextSibling;
          v2.insertBefore(o13, d3), o13 = l9;
        }
      }
    }
    return n9;
  };
  var f6 = (o12, l8, i5 = o12) => (o12._$AI(l8, i5), o12);
  var s5 = {};
  var a3 = (o12, l8 = s5) => o12._$AH = l8;
  var m2 = (o12) => o12._$AH;
  var p10 = (o12) => {
    var l8;
    null === (l8 = o12._$AP) || void 0 === l8 || l8.call(o12, false, true);
    let i5 = o12._$AA;
    const n9 = o12._$AB.nextSibling;
    for (; i5 !== n9; ) {
      const o13 = i5.nextSibling;
      i5.remove(), i5 = o13;
    }
  };

  // ../../../../../../../../Dropbox/projects/hazel/node_modules/lit-html/directives/repeat.js
  var u3 = (e11, s7, t5) => {
    const r6 = /* @__PURE__ */ new Map();
    for (let l8 = s7; l8 <= t5; l8++) r6.set(e11[l8], l8);
    return r6;
  };
  var c4 = e7(class extends i4 {
    constructor(e11) {
      if (super(e11), e11.type !== t4.CHILD) throw Error("repeat() can only be used in text expressions");
    }
    ct(e11, s7, t5) {
      let r6;
      void 0 === t5 ? t5 = s7 : void 0 !== s7 && (r6 = s7);
      const l8 = [], o12 = [];
      let i5 = 0;
      for (const s8 of e11) l8[i5] = r6 ? r6(s8, i5) : i5, o12[i5] = t5(s8, i5), i5++;
      return { values: o12, keys: l8 };
    }
    render(e11, s7, t5) {
      return this.ct(e11, s7, t5).values;
    }
    update(s7, [t5, r6, c6]) {
      var d3;
      const a4 = m2(s7), { values: p11, keys: v2 } = this.ct(t5, r6, c6);
      if (!Array.isArray(a4)) return this.ut = v2, p11;
      const h5 = null !== (d3 = this.ut) && void 0 !== d3 ? d3 : this.ut = [], m3 = [];
      let y2, x2, j2 = 0, k2 = a4.length - 1, w2 = 0, A2 = p11.length - 1;
      for (; j2 <= k2 && w2 <= A2; ) if (null === a4[j2]) j2++;
      else if (null === a4[k2]) k2--;
      else if (h5[j2] === v2[w2]) m3[w2] = f6(a4[j2], p11[w2]), j2++, w2++;
      else if (h5[k2] === v2[A2]) m3[A2] = f6(a4[k2], p11[A2]), k2--, A2--;
      else if (h5[j2] === v2[A2]) m3[A2] = f6(a4[j2], p11[A2]), c3(s7, m3[A2 + 1], a4[j2]), j2++, A2--;
      else if (h5[k2] === v2[w2]) m3[w2] = f6(a4[k2], p11[w2]), c3(s7, a4[j2], a4[k2]), k2--, w2++;
      else if (void 0 === y2 && (y2 = u3(v2, w2, A2), x2 = u3(h5, j2, k2)), y2.has(h5[j2])) if (y2.has(h5[k2])) {
        const e11 = x2.get(v2[w2]), t6 = void 0 !== e11 ? a4[e11] : null;
        if (null === t6) {
          const e12 = c3(s7, a4[j2]);
          f6(e12, p11[w2]), m3[w2] = e12;
        } else m3[w2] = f6(t6, p11[w2]), c3(s7, a4[j2], t6), a4[e11] = null;
        w2++;
      } else p10(a4[k2]), k2--;
      else p10(a4[j2]), j2++;
      for (; w2 <= A2; ) {
        const e11 = c3(s7, m3[A2 + 1]);
        f6(e11, p11[w2]), m3[w2++] = e11;
      }
      for (; j2 <= k2; ) {
        const e11 = a4[j2++];
        null !== e11 && p10(e11);
      }
      return this.ut = v2, a3(s7, m3), T;
    }
  });

  // ../../../../../../../../Dropbox/projects/hazel/node_modules/lit-html/directives/live.js
  var l6 = e7(class extends i4 {
    constructor(r6) {
      if (super(r6), r6.type !== t4.PROPERTY && r6.type !== t4.ATTRIBUTE && r6.type !== t4.BOOLEAN_ATTRIBUTE) throw Error("The `live` directive is not allowed on child or event bindings");
      if (!e8(r6)) throw Error("`live` bindings can only contain a single expression");
    }
    render(r6) {
      return r6;
    }
    update(i5, [t5]) {
      if (t5 === T || t5 === A) return t5;
      const o12 = i5.element, l8 = i5.name;
      if (i5.type === t4.PROPERTY) {
        if (t5 === o12[l8]) return T;
      } else if (i5.type === t4.BOOLEAN_ATTRIBUTE) {
        if (!!t5 === o12.hasAttribute(l8)) return T;
      } else if (i5.type === t4.ATTRIBUTE && o12.getAttribute(l8) === t5 + "") return T;
      return a3(i5), t5;
    }
  });

  // ../../../../../../../../Dropbox/projects/hazel/node_modules/lit-html/async-directive.js
  var s6 = (i5, t5) => {
    var e11, o12;
    const r6 = i5._$AN;
    if (void 0 === r6) return false;
    for (const i6 of r6) null === (o12 = (e11 = i6)._$AO) || void 0 === o12 || o12.call(e11, t5, false), s6(i6, t5);
    return true;
  };
  var o7 = (i5) => {
    let t5, e11;
    do {
      if (void 0 === (t5 = i5._$AM)) break;
      e11 = t5._$AN, e11.delete(i5), i5 = t5;
    } while (0 === (null == e11 ? void 0 : e11.size));
  };
  var r5 = (i5) => {
    for (let t5; t5 = i5._$AM; i5 = t5) {
      let e11 = t5._$AN;
      if (void 0 === e11) t5._$AN = e11 = /* @__PURE__ */ new Set();
      else if (e11.has(i5)) break;
      e11.add(i5), l7(t5);
    }
  };
  function n7(i5) {
    void 0 !== this._$AN ? (o7(this), this._$AM = i5, r5(this)) : this._$AM = i5;
  }
  function h3(i5, t5 = false, e11 = 0) {
    const r6 = this._$AH, n9 = this._$AN;
    if (void 0 !== n9 && 0 !== n9.size) if (t5) if (Array.isArray(r6)) for (let i6 = e11; i6 < r6.length; i6++) s6(r6[i6], false), o7(r6[i6]);
    else null != r6 && (s6(r6, false), o7(r6));
    else s6(this, i5);
  }
  var l7 = (i5) => {
    var t5, s7, o12, r6;
    i5.type == t4.CHILD && (null !== (t5 = (o12 = i5)._$AP) && void 0 !== t5 || (o12._$AP = h3), null !== (s7 = (r6 = i5)._$AQ) && void 0 !== s7 || (r6._$AQ = n7));
  };
  var c5 = class extends i4 {
    constructor() {
      super(...arguments), this._$AN = void 0;
    }
    _$AT(i5, t5, e11) {
      super._$AT(i5, t5, e11), r5(this), this.isConnected = i5._$AU;
    }
    _$AO(i5, t5 = true) {
      var e11, r6;
      i5 !== this.isConnected && (this.isConnected = i5, i5 ? null === (e11 = this.reconnected) || void 0 === e11 || e11.call(this) : null === (r6 = this.disconnected) || void 0 === r6 || r6.call(this)), t5 && (s6(this, i5), o7(this));
    }
    setValue(t5) {
      if (e8(this._$Ct)) this._$Ct._$AI(t5, this);
      else {
        const i5 = [...this._$Ct._$AH];
        i5[this._$Ci] = t5, this._$Ct._$AI(i5, this, 0);
      }
    }
    disconnected() {
    }
    reconnected() {
    }
  };

  // ../../../../../../../../Dropbox/projects/hazel/node_modules/lit-html/directives/ref.js
  var e9 = () => new o8();
  var o8 = class {
  };
  var h4 = /* @__PURE__ */ new WeakMap();
  var n8 = e7(class extends c5 {
    render(t5) {
      return A;
    }
    update(t5, [s7]) {
      var e11;
      const o12 = s7 !== this.G;
      return o12 && void 0 !== this.G && this.ot(void 0), (o12 || this.rt !== this.lt) && (this.G = s7, this.dt = null === (e11 = t5.options) || void 0 === e11 ? void 0 : e11.host, this.ot(this.lt = t5.element)), A;
    }
    ot(i5) {
      var t5;
      if ("function" == typeof this.G) {
        const s7 = null !== (t5 = this.dt) && void 0 !== t5 ? t5 : globalThis;
        let e11 = h4.get(s7);
        void 0 === e11 && (e11 = /* @__PURE__ */ new WeakMap(), h4.set(s7, e11)), void 0 !== e11.get(this.G) && this.G.call(this.dt, void 0), e11.set(this.G, i5), void 0 !== i5 && this.G.call(this.dt, i5);
      } else this.G.value = i5;
    }
    get rt() {
      var i5, t5, s7;
      return "function" == typeof this.G ? null === (t5 = h4.get(null !== (i5 = this.dt) && void 0 !== i5 ? i5 : globalThis)) || void 0 === t5 ? void 0 : t5.get(this.G) : null === (s7 = this.G) || void 0 === s7 ? void 0 : s7.value;
    }
    disconnected() {
      this.rt === this.lt && this.ot(void 0);
    }
    reconnected() {
      this.ot(this.lt);
    }
  });

  // ../../../../../../../../Dropbox/projects/hazel/node_modules/lit-html/directives/class-map.js
  var o9 = e7(class extends i4 {
    constructor(t5) {
      var i5;
      if (super(t5), t5.type !== t4.ATTRIBUTE || "class" !== t5.name || (null === (i5 = t5.strings) || void 0 === i5 ? void 0 : i5.length) > 2) throw Error("`classMap()` can only be used in the `class` attribute and must be the only part in the attribute.");
    }
    render(t5) {
      return " " + Object.keys(t5).filter((i5) => t5[i5]).join(" ") + " ";
    }
    update(i5, [s7]) {
      var r6, o12;
      if (void 0 === this.it) {
        this.it = /* @__PURE__ */ new Set(), void 0 !== i5.strings && (this.nt = new Set(i5.strings.join(" ").split(/\s/).filter((t5) => "" !== t5)));
        for (const t5 in s7) s7[t5] && !(null === (r6 = this.nt) || void 0 === r6 ? void 0 : r6.has(t5)) && this.it.add(t5);
        return this.render(s7);
      }
      const e11 = i5.element.classList;
      this.it.forEach((t5) => {
        t5 in s7 || (e11.remove(t5), this.it.delete(t5));
      });
      for (const t5 in s7) {
        const i6 = !!s7[t5];
        i6 === this.it.has(t5) || (null === (o12 = this.nt) || void 0 === o12 ? void 0 : o12.has(t5)) || (i6 ? (e11.add(t5), this.it.add(t5)) : (e11.remove(t5), this.it.delete(t5)));
      }
      return T;
    }
  });

  // ../../../../../../../../Dropbox/projects/hazel/node_modules/hotkeys-js/dist/hotkeys.esm.js
  var isff = typeof navigator !== "undefined" ? navigator.userAgent.toLowerCase().indexOf("firefox") > 0 : false;
  function addEvent(object, event, method) {
    if (object.addEventListener) {
      object.addEventListener(event, method, false);
    } else if (object.attachEvent) {
      object.attachEvent("on".concat(event), function() {
        method(window.event);
      });
    }
  }
  function getMods(modifier, key) {
    var mods = key.slice(0, key.length - 1);
    for (var i5 = 0; i5 < mods.length; i5++) {
      mods[i5] = modifier[mods[i5].toLowerCase()];
    }
    return mods;
  }
  function getKeys(key) {
    if (typeof key !== "string") key = "";
    key = key.replace(/\s/g, "");
    var keys = key.split(",");
    var index = keys.lastIndexOf("");
    for (; index >= 0; ) {
      keys[index - 1] += ",";
      keys.splice(index, 1);
      index = keys.lastIndexOf("");
    }
    return keys;
  }
  function compareArray(a1, a22) {
    var arr1 = a1.length >= a22.length ? a1 : a22;
    var arr2 = a1.length >= a22.length ? a22 : a1;
    var isIndex = true;
    for (var i5 = 0; i5 < arr1.length; i5++) {
      if (arr2.indexOf(arr1[i5]) === -1) isIndex = false;
    }
    return isIndex;
  }
  var _keyMap = {
    backspace: 8,
    tab: 9,
    clear: 12,
    enter: 13,
    return: 13,
    esc: 27,
    escape: 27,
    space: 32,
    left: 37,
    up: 38,
    right: 39,
    down: 40,
    del: 46,
    delete: 46,
    ins: 45,
    insert: 45,
    home: 36,
    end: 35,
    pageup: 33,
    pagedown: 34,
    capslock: 20,
    num_0: 96,
    num_1: 97,
    num_2: 98,
    num_3: 99,
    num_4: 100,
    num_5: 101,
    num_6: 102,
    num_7: 103,
    num_8: 104,
    num_9: 105,
    num_multiply: 106,
    num_add: 107,
    num_enter: 108,
    num_subtract: 109,
    num_decimal: 110,
    num_divide: 111,
    "\u21EA": 20,
    ",": 188,
    ".": 190,
    "/": 191,
    "`": 192,
    "-": isff ? 173 : 189,
    "=": isff ? 61 : 187,
    ";": isff ? 59 : 186,
    "'": 222,
    "[": 219,
    "]": 221,
    "\\": 220
  };
  var _modifier = {
    // shiftKey
    "\u21E7": 16,
    shift: 16,
    // altKey
    "\u2325": 18,
    alt: 18,
    option: 18,
    // ctrlKey
    "\u2303": 17,
    ctrl: 17,
    control: 17,
    // metaKey
    "\u2318": 91,
    cmd: 91,
    command: 91
  };
  var modifierMap = {
    16: "shiftKey",
    18: "altKey",
    17: "ctrlKey",
    91: "metaKey",
    shiftKey: 16,
    ctrlKey: 17,
    altKey: 18,
    metaKey: 91
  };
  var _mods = {
    16: false,
    18: false,
    17: false,
    91: false
  };
  var _handlers = {};
  for (k2 = 1; k2 < 20; k2++) {
    _keyMap["f".concat(k2)] = 111 + k2;
  }
  var k2;
  var _downKeys = [];
  var _scope = "all";
  var elementHasBindEvent = [];
  var code = function code2(x2) {
    return _keyMap[x2.toLowerCase()] || _modifier[x2.toLowerCase()] || x2.toUpperCase().charCodeAt(0);
  };
  function setScope(scope) {
    _scope = scope || "all";
  }
  function getScope() {
    return _scope || "all";
  }
  function getPressedKeyCodes() {
    return _downKeys.slice(0);
  }
  function filter2(event) {
    var target = event.target || event.srcElement;
    var tagName = target.tagName;
    var flag2 = true;
    if (target.isContentEditable || (tagName === "INPUT" || tagName === "TEXTAREA" || tagName === "SELECT") && !target.readOnly) {
      flag2 = false;
    }
    return flag2;
  }
  function isPressed(keyCode) {
    if (typeof keyCode === "string") {
      keyCode = code(keyCode);
    }
    return _downKeys.indexOf(keyCode) !== -1;
  }
  function deleteScope(scope, newScope) {
    var handlers;
    var i5;
    if (!scope) scope = getScope();
    for (var key in _handlers) {
      if (Object.prototype.hasOwnProperty.call(_handlers, key)) {
        handlers = _handlers[key];
        for (i5 = 0; i5 < handlers.length; ) {
          if (handlers[i5].scope === scope) handlers.splice(i5, 1);
          else i5++;
        }
      }
    }
    if (getScope() === scope) setScope(newScope || "all");
  }
  function clearModifier(event) {
    var key = event.keyCode || event.which || event.charCode;
    var i5 = _downKeys.indexOf(key);
    if (i5 >= 0) {
      _downKeys.splice(i5, 1);
    }
    if (event.key && event.key.toLowerCase() === "meta") {
      _downKeys.splice(0, _downKeys.length);
    }
    if (key === 93 || key === 224) key = 91;
    if (key in _mods) {
      _mods[key] = false;
      for (var k2 in _modifier) {
        if (_modifier[k2] === key) hotkeys[k2] = false;
      }
    }
  }
  function unbind(keysInfo) {
    if (!keysInfo) {
      Object.keys(_handlers).forEach(function(key) {
        return delete _handlers[key];
      });
    } else if (Array.isArray(keysInfo)) {
      keysInfo.forEach(function(info) {
        if (info.key) eachUnbind(info);
      });
    } else if (typeof keysInfo === "object") {
      if (keysInfo.key) eachUnbind(keysInfo);
    } else if (typeof keysInfo === "string") {
      for (var _len = arguments.length, args2 = new Array(_len > 1 ? _len - 1 : 0), _key = 1; _key < _len; _key++) {
        args2[_key - 1] = arguments[_key];
      }
      var scope = args2[0], method = args2[1];
      if (typeof scope === "function") {
        method = scope;
        scope = "";
      }
      eachUnbind({
        key: keysInfo,
        scope,
        method,
        splitKey: "+"
      });
    }
  }
  var eachUnbind = function eachUnbind2(_ref) {
    var key = _ref.key, scope = _ref.scope, method = _ref.method, _ref$splitKey = _ref.splitKey, splitKey = _ref$splitKey === void 0 ? "+" : _ref$splitKey;
    var multipleKeys = getKeys(key);
    multipleKeys.forEach(function(originKey) {
      var unbindKeys = originKey.split(splitKey);
      var len = unbindKeys.length;
      var lastKey = unbindKeys[len - 1];
      var keyCode = lastKey === "*" ? "*" : code(lastKey);
      if (!_handlers[keyCode]) return;
      if (!scope) scope = getScope();
      var mods = len > 1 ? getMods(_modifier, unbindKeys) : [];
      _handlers[keyCode] = _handlers[keyCode].map(function(record) {
        var isMatchingMethod = method ? record.method === method : true;
        if (isMatchingMethod && record.scope === scope && compareArray(record.mods, mods)) {
          return {};
        }
        return record;
      });
    });
  };
  function eventHandler(event, handler, scope) {
    var modifiersMatch;
    if (handler.scope === scope || handler.scope === "all") {
      modifiersMatch = handler.mods.length > 0;
      for (var y2 in _mods) {
        if (Object.prototype.hasOwnProperty.call(_mods, y2)) {
          if (!_mods[y2] && handler.mods.indexOf(+y2) > -1 || _mods[y2] && handler.mods.indexOf(+y2) === -1) {
            modifiersMatch = false;
          }
        }
      }
      if (handler.mods.length === 0 && !_mods[16] && !_mods[18] && !_mods[17] && !_mods[91] || modifiersMatch || handler.shortcut === "*") {
        if (handler.method(event, handler) === false) {
          if (event.preventDefault) event.preventDefault();
          else event.returnValue = false;
          if (event.stopPropagation) event.stopPropagation();
          if (event.cancelBubble) event.cancelBubble = true;
        }
      }
    }
  }
  function dispatch(event) {
    var asterisk = _handlers["*"];
    var key = event.keyCode || event.which || event.charCode;
    if (!hotkeys.filter.call(this, event)) return;
    if (key === 93 || key === 224) key = 91;
    if (_downKeys.indexOf(key) === -1 && key !== 229) _downKeys.push(key);
    ["ctrlKey", "altKey", "shiftKey", "metaKey"].forEach(function(keyName) {
      var keyNum = modifierMap[keyName];
      if (event[keyName] && _downKeys.indexOf(keyNum) === -1) {
        _downKeys.push(keyNum);
      } else if (!event[keyName] && _downKeys.indexOf(keyNum) > -1) {
        _downKeys.splice(_downKeys.indexOf(keyNum), 1);
      } else if (keyName === "metaKey" && event[keyName] && _downKeys.length === 3) {
        if (!(event.ctrlKey || event.shiftKey || event.altKey)) {
          _downKeys = _downKeys.slice(_downKeys.indexOf(keyNum));
        }
      }
    });
    if (key in _mods) {
      _mods[key] = true;
      for (var k2 in _modifier) {
        if (_modifier[k2] === key) hotkeys[k2] = true;
      }
      if (!asterisk) return;
    }
    for (var e11 in _mods) {
      if (Object.prototype.hasOwnProperty.call(_mods, e11)) {
        _mods[e11] = event[modifierMap[e11]];
      }
    }
    if (event.getModifierState && !(event.altKey && !event.ctrlKey) && event.getModifierState("AltGraph")) {
      if (_downKeys.indexOf(17) === -1) {
        _downKeys.push(17);
      }
      if (_downKeys.indexOf(18) === -1) {
        _downKeys.push(18);
      }
      _mods[17] = true;
      _mods[18] = true;
    }
    var scope = getScope();
    if (asterisk) {
      for (var i5 = 0; i5 < asterisk.length; i5++) {
        if (asterisk[i5].scope === scope && (event.type === "keydown" && asterisk[i5].keydown || event.type === "keyup" && asterisk[i5].keyup)) {
          eventHandler(event, asterisk[i5], scope);
        }
      }
    }
    if (!(key in _handlers)) return;
    for (var _i = 0; _i < _handlers[key].length; _i++) {
      if (event.type === "keydown" && _handlers[key][_i].keydown || event.type === "keyup" && _handlers[key][_i].keyup) {
        if (_handlers[key][_i].key) {
          var record = _handlers[key][_i];
          var splitKey = record.splitKey;
          var keyShortcut = record.key.split(splitKey);
          var _downKeysCurrent = [];
          for (var a4 = 0; a4 < keyShortcut.length; a4++) {
            _downKeysCurrent.push(code(keyShortcut[a4]));
          }
          if (_downKeysCurrent.sort().join("") === _downKeys.sort().join("")) {
            eventHandler(event, record, scope);
          }
        }
      }
    }
  }
  function isElementBind(element) {
    return elementHasBindEvent.indexOf(element) > -1;
  }
  function hotkeys(key, option, method) {
    _downKeys = [];
    var keys = getKeys(key);
    var mods = [];
    var scope = "all";
    var element = document;
    var i5 = 0;
    var keyup = false;
    var keydown = true;
    var splitKey = "+";
    if (method === void 0 && typeof option === "function") {
      method = option;
    }
    if (Object.prototype.toString.call(option) === "[object Object]") {
      if (option.scope) scope = option.scope;
      if (option.element) element = option.element;
      if (option.keyup) keyup = option.keyup;
      if (option.keydown !== void 0) keydown = option.keydown;
      if (typeof option.splitKey === "string") splitKey = option.splitKey;
    }
    if (typeof option === "string") scope = option;
    for (; i5 < keys.length; i5++) {
      key = keys[i5].split(splitKey);
      mods = [];
      if (key.length > 1) mods = getMods(_modifier, key);
      key = key[key.length - 1];
      key = key === "*" ? "*" : code(key);
      if (!(key in _handlers)) _handlers[key] = [];
      _handlers[key].push({
        keyup,
        keydown,
        scope,
        mods,
        shortcut: keys[i5],
        method,
        key: keys[i5],
        splitKey
      });
    }
    if (typeof element !== "undefined" && !isElementBind(element) && window) {
      elementHasBindEvent.push(element);
      addEvent(element, "keydown", function(e11) {
        dispatch(e11);
      });
      addEvent(window, "focus", function() {
        _downKeys = [];
      });
      addEvent(element, "keyup", function(e11) {
        dispatch(e11);
        clearModifier(e11);
      });
    }
  }
  var _api = {
    setScope,
    getScope,
    deleteScope,
    getPressedKeyCodes,
    isPressed,
    filter: filter2,
    unbind
  };
  for (a4 in _api) {
    if (Object.prototype.hasOwnProperty.call(_api, a4)) {
      hotkeys[a4] = _api[a4];
    }
  }
  var a4;
  if (typeof window !== "undefined") {
    _hotkeys = window.hotkeys;
    hotkeys.noConflict = function(deep) {
      if (deep && window.hotkeys === hotkeys) {
        window.hotkeys = _hotkeys;
      }
      return hotkeys;
    };
    window.hotkeys = hotkeys;
  }
  var _hotkeys;
  var hotkeys_esm_default = hotkeys;

  // ../../../../../../../../Dropbox/projects/hazel/node_modules/ninja-keys/dist/ninja-header.js
  var __decorate = function(decorators, target, key, desc) {
    var c6 = arguments.length, r6 = c6 < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d3;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r6 = Reflect.decorate(decorators, target, key, desc);
    else for (var i5 = decorators.length - 1; i5 >= 0; i5--) if (d3 = decorators[i5]) r6 = (c6 < 3 ? d3(r6) : c6 > 3 ? d3(target, key, r6) : d3(target, key)) || r6;
    return c6 > 3 && r6 && Object.defineProperty(target, key, r6), r6;
  };
  var NinjaHeader = class NinjaHeader2 extends s4 {
    constructor() {
      super(...arguments);
      this.placeholder = "";
      this.hideBreadcrumbs = false;
      this.breadcrumbHome = "Home";
      this.breadcrumbs = [];
      this._inputRef = e9();
    }
    render() {
      let breadcrumbs = "";
      if (!this.hideBreadcrumbs) {
        const itemTemplates = [];
        for (const breadcrumb of this.breadcrumbs) {
          itemTemplates.push(x`<button
            tabindex="-1"
            @click=${() => this.selectParent(breadcrumb)}
            class="breadcrumb"
          >
            ${breadcrumb}
          </button>`);
        }
        breadcrumbs = x`<div class="breadcrumb-list">
        <button
          tabindex="-1"
          @click=${() => this.selectParent()}
          class="breadcrumb"
        >
          ${this.breadcrumbHome}
        </button>
        ${itemTemplates}
      </div>`;
      }
      return x`
      ${breadcrumbs}
      <div part="ninja-input-wrapper" class="search-wrapper">
        <input
          part="ninja-input"
          type="text"
          id="search"
          spellcheck="false"
          autocomplete="off"
          @input="${this._handleInput}"
          ${n8(this._inputRef)}
          placeholder="${this.placeholder}"
          class="search"
        />
      </div>
    `;
    }
    setSearch(value) {
      if (this._inputRef.value) {
        this._inputRef.value.value = value;
      }
    }
    focusSearch() {
      requestAnimationFrame(() => this._inputRef.value.focus());
    }
    _handleInput(event) {
      const input = event.target;
      this.dispatchEvent(new CustomEvent("change", {
        detail: { search: input.value },
        bubbles: false,
        composed: false
      }));
    }
    selectParent(breadcrumb) {
      this.dispatchEvent(new CustomEvent("setParent", {
        detail: { parent: breadcrumb },
        bubbles: true,
        composed: true
      }));
    }
    firstUpdated() {
      this.focusSearch();
    }
    _close() {
      this.dispatchEvent(new CustomEvent("close", { bubbles: true, composed: true }));
    }
  };
  NinjaHeader.styles = i`
    :host {
      flex: 1;
      position: relative;
    }
    .search {
      padding: 1.25em;
      flex-grow: 1;
      flex-shrink: 0;
      margin: 0px;
      border: none;
      appearance: none;
      font-size: 1.125em;
      background: transparent;
      caret-color: var(--ninja-accent-color);
      color: var(--ninja-text-color);
      outline: none;
      font-family: var(--ninja-font-family);
    }
    .search::placeholder {
      color: var(--ninja-placeholder-color);
    }
    .breadcrumb-list {
      padding: 1em 4em 0 1em;
      display: flex;
      flex-direction: row;
      align-items: stretch;
      justify-content: flex-start;
      flex: initial;
    }

    .breadcrumb {
      background: var(--ninja-secondary-background-color);
      text-align: center;
      line-height: 1.2em;
      border-radius: var(--ninja-key-border-radius);
      border: 0;
      cursor: pointer;
      padding: 0.1em 0.5em;
      color: var(--ninja-secondary-text-color);
      margin-right: 0.5em;
      outline: none;
      font-family: var(--ninja-font-family);
    }

    .search-wrapper {
      display: flex;
      border-bottom: var(--ninja-separate-border);
    }
  `;
  __decorate([
    n5()
  ], NinjaHeader.prototype, "placeholder", void 0);
  __decorate([
    n5({ type: Boolean })
  ], NinjaHeader.prototype, "hideBreadcrumbs", void 0);
  __decorate([
    n5()
  ], NinjaHeader.prototype, "breadcrumbHome", void 0);
  __decorate([
    n5({ type: Array })
  ], NinjaHeader.prototype, "breadcrumbs", void 0);
  NinjaHeader = __decorate([
    e4("ninja-header")
  ], NinjaHeader);

  // ../../../../../../../../Dropbox/projects/hazel/node_modules/lit-html/directives/unsafe-html.js
  var e10 = class extends i4 {
    constructor(i5) {
      if (super(i5), this.et = A, i5.type !== t4.CHILD) throw Error(this.constructor.directiveName + "() can only be used in child bindings");
    }
    render(r6) {
      if (r6 === A || null == r6) return this.ft = void 0, this.et = r6;
      if (r6 === T) return r6;
      if ("string" != typeof r6) throw Error(this.constructor.directiveName + "() called with a non-string value");
      if (r6 === this.et) return this.ft;
      this.et = r6;
      const s7 = [r6];
      return s7.raw = s7, this.ft = { _$litType$: this.constructor.resultType, strings: s7, values: [] };
    }
  };
  e10.directiveName = "unsafeHTML", e10.resultType = 1;
  var o10 = e7(e10);

  // ../../../../../../../../Dropbox/projects/hazel/node_modules/lit-html/directives/join.js
  function* o11(o12, t5) {
    const f7 = "function" == typeof t5;
    if (void 0 !== o12) {
      let i5 = -1;
      for (const n9 of o12) i5 > -1 && (yield f7 ? t5(i5) : t5), i5++, yield n9;
    }
  }

  // ../../../../../../../../Dropbox/projects/hazel/node_modules/tslib/tslib.es6.mjs
  function __decorate2(decorators, target, key, desc) {
    var c6 = arguments.length, r6 = c6 < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d3;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r6 = Reflect.decorate(decorators, target, key, desc);
    else for (var i5 = decorators.length - 1; i5 >= 0; i5--) if (d3 = decorators[i5]) r6 = (c6 < 3 ? d3(r6) : c6 > 3 ? d3(target, key, r6) : d3(target, key)) || r6;
    return c6 > 3 && r6 && Object.defineProperty(target, key, r6), r6;
  }

  // ../../../../../../../../Dropbox/projects/hazel/node_modules/@material/mwc-icon/mwc-icon-host.css.js
  var styles = i`:host{font-family:var(--mdc-icon-font, "Material Icons");font-weight:normal;font-style:normal;font-size:var(--mdc-icon-size, 24px);line-height:1;letter-spacing:normal;text-transform:none;display:inline-block;white-space:nowrap;word-wrap:normal;direction:ltr;-webkit-font-smoothing:antialiased;text-rendering:optimizeLegibility;-moz-osx-font-smoothing:grayscale;font-feature-settings:"liga"}`;

  // ../../../../../../../../Dropbox/projects/hazel/node_modules/@material/mwc-icon/mwc-icon.js
  var Icon = class Icon2 extends s4 {
    /** @soyTemplate */
    render() {
      return x`<span><slot></slot></span>`;
    }
  };
  Icon.styles = [styles];
  Icon = __decorate2([
    e4("mwc-icon")
  ], Icon);

  // ../../../../../../../../Dropbox/projects/hazel/node_modules/ninja-keys/dist/ninja-action.js
  var __decorate3 = function(decorators, target, key, desc) {
    var c6 = arguments.length, r6 = c6 < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d3;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r6 = Reflect.decorate(decorators, target, key, desc);
    else for (var i5 = decorators.length - 1; i5 >= 0; i5--) if (d3 = decorators[i5]) r6 = (c6 < 3 ? d3(r6) : c6 > 3 ? d3(target, key, r6) : d3(target, key)) || r6;
    return c6 > 3 && r6 && Object.defineProperty(target, key, r6), r6;
  };
  var NinjaAction = class NinjaAction2 extends s4 {
    constructor() {
      super();
      this.selected = false;
      this.hotKeysJoinedView = true;
      this.addEventListener("click", this.click);
    }
    /**
     * Scroll to show element
     */
    ensureInView() {
      requestAnimationFrame(() => this.scrollIntoView({ block: "nearest" }));
    }
    click() {
      this.dispatchEvent(new CustomEvent("actionsSelected", {
        detail: this.action,
        bubbles: true,
        composed: true
      }));
    }
    updated(changedProperties) {
      if (changedProperties.has("selected")) {
        if (this.selected) {
          this.ensureInView();
        }
      }
    }
    render() {
      let icon;
      if (this.action.mdIcon) {
        icon = x`<mwc-icon part="ninja-icon" class="ninja-icon"
        >${this.action.mdIcon}</mwc-icon
      >`;
      } else if (this.action.icon) {
        icon = o10(this.action.icon || "");
      }
      let hotkey;
      if (this.action.hotkey) {
        if (this.hotKeysJoinedView) {
          hotkey = this.action.hotkey.split(",").map((hotkeys2) => {
            const keys = hotkeys2.split("+");
            const joinedKeys = x`${o11(keys.map((key) => x`<kbd>${key}</kbd>`), "+")}`;
            return x`<div class="ninja-hotkey ninja-hotkeys">
            ${joinedKeys}
          </div>`;
          });
        } else {
          hotkey = this.action.hotkey.split(",").map((hotkeys2) => {
            const keys = hotkeys2.split("+");
            const keyElements = keys.map((key) => x`<kbd class="ninja-hotkey">${key}</kbd>`);
            return x`<kbd class="ninja-hotkeys">${keyElements}</kbd>`;
          });
        }
      }
      const classes = {
        selected: this.selected,
        "ninja-action": true
      };
      return x`
      <div
        class="ninja-action"
        part="ninja-action ${this.selected ? "ninja-selected" : ""}"
        class=${o9(classes)}
      >
        ${icon}
        <div class="ninja-title">${this.action.title}</div>
        ${hotkey}
      </div>
    `;
    }
  };
  NinjaAction.styles = i`
    :host {
      display: flex;
      width: 100%;
    }
    .ninja-action {
      padding: 0.75em 1em;
      display: flex;
      border-left: 2px solid transparent;
      align-items: center;
      justify-content: start;
      outline: none;
      transition: color 0s ease 0s;
      width: 100%;
    }
    .ninja-action.selected {
      cursor: pointer;
      color: var(--ninja-selected-text-color);
      background-color: var(--ninja-selected-background);
      border-left: 2px solid var(--ninja-accent-color);
      outline: none;
    }
    .ninja-action.selected .ninja-icon {
      color: var(--ninja-selected-text-color);
    }
    .ninja-icon {
      font-size: var(--ninja-icon-size);
      max-width: var(--ninja-icon-size);
      max-height: var(--ninja-icon-size);
      margin-right: 1em;
      color: var(--ninja-icon-color);
      margin-right: 1em;
      position: relative;
    }

    .ninja-title {
      flex-shrink: 0.01;
      margin-right: 0.5em;
      flex-grow: 1;
      font-size: 0.8125em;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
    }
    .ninja-hotkeys {
      flex-shrink: 0;
      width: min-content;
      display: flex;
    }

    .ninja-hotkeys kbd {
      font-family: inherit;
    }
    .ninja-hotkey {
      background: var(--ninja-secondary-background-color);
      padding: 0.06em 0.25em;
      border-radius: var(--ninja-key-border-radius);
      text-transform: capitalize;
      color: var(--ninja-secondary-text-color);
      font-size: 0.75em;
      font-family: inherit;
    }

    .ninja-hotkey + .ninja-hotkey {
      margin-left: 0.5em;
    }
    .ninja-hotkeys + .ninja-hotkeys {
      margin-left: 1em;
    }
  `;
  __decorate3([
    n5({ type: Object })
  ], NinjaAction.prototype, "action", void 0);
  __decorate3([
    n5({ type: Boolean })
  ], NinjaAction.prototype, "selected", void 0);
  __decorate3([
    n5({ type: Boolean })
  ], NinjaAction.prototype, "hotKeysJoinedView", void 0);
  NinjaAction = __decorate3([
    e4("ninja-action")
  ], NinjaAction);

  // ../../../../../../../../Dropbox/projects/hazel/node_modules/ninja-keys/dist/ninja-footer.js
  var footerHtml = x` <div class="modal-footer" slot="footer">
  <span class="help">
    <svg
      version="1.0"
      class="ninja-examplekey"
      xmlns="http://www.w3.org/2000/svg"
      viewBox="0 0 1280 1280"
    >
      <path
        d="M1013 376c0 73.4-.4 113.3-1.1 120.2a159.9 159.9 0 0 1-90.2 127.3c-20 9.6-36.7 14-59.2 15.5-7.1.5-121.9.9-255 1h-242l95.5-95.5 95.5-95.5-38.3-38.2-38.2-38.3-160 160c-88 88-160 160.4-160 161 0 .6 72 73 160 161l160 160 38.2-38.3 38.3-38.2-95.5-95.5-95.5-95.5h251.1c252.9 0 259.8-.1 281.4-3.6 72.1-11.8 136.9-54.1 178.5-116.4 8.6-12.9 22.6-40.5 28-55.4 4.4-12 10.7-36.1 13.1-50.6 1.6-9.6 1.8-21 2.1-132.8l.4-122.2H1013v110z"
      />
    </svg>

    to select
  </span>
  <span class="help">
    <svg
      xmlns="http://www.w3.org/2000/svg"
      class="ninja-examplekey"
      viewBox="0 0 24 24"
    >
      <path d="M0 0h24v24H0V0z" fill="none" />
      <path
        d="M20 12l-1.41-1.41L13 16.17V4h-2v12.17l-5.58-5.59L4 12l8 8 8-8z"
      />
    </svg>
    <svg
      xmlns="http://www.w3.org/2000/svg"
      class="ninja-examplekey"
      viewBox="0 0 24 24"
    >
      <path d="M0 0h24v24H0V0z" fill="none" />
      <path d="M4 12l1.41 1.41L11 7.83V20h2V7.83l5.58 5.59L20 12l-8-8-8 8z" />
    </svg>
    to navigate
  </span>
  <span class="help">
    <span class="ninja-examplekey esc">esc</span>
    to close
  </span>
  <span class="help">
    <svg
      xmlns="http://www.w3.org/2000/svg"
      class="ninja-examplekey backspace"
      viewBox="0 0 20 20"
      fill="currentColor"
    >
      <path
        fill-rule="evenodd"
        d="M6.707 4.879A3 3 0 018.828 4H15a3 3 0 013 3v6a3 3 0 01-3 3H8.828a3 3 0 01-2.12-.879l-4.415-4.414a1 1 0 010-1.414l4.414-4.414zm4 2.414a1 1 0 00-1.414 1.414L10.586 10l-1.293 1.293a1 1 0 101.414 1.414L12 11.414l1.293 1.293a1 1 0 001.414-1.414L13.414 10l1.293-1.293a1 1 0 00-1.414-1.414L12 8.586l-1.293-1.293z"
        clip-rule="evenodd"
      />
    </svg>
    move to parent
  </span>
</div>`;

  // ../../../../../../../../Dropbox/projects/hazel/node_modules/ninja-keys/dist/base-styles.js
  var baseStyles = i`
  :host {
    --ninja-width: 640px;
    --ninja-backdrop-filter: none;
    --ninja-overflow-background: rgba(255, 255, 255, 0.5);
    --ninja-text-color: rgb(60, 65, 73);
    --ninja-font-size: 16px;
    --ninja-top: 20%;

    --ninja-key-border-radius: 0.25em;
    --ninja-accent-color: rgb(110, 94, 210);
    --ninja-secondary-background-color: rgb(239, 241, 244);
    --ninja-secondary-text-color: rgb(107, 111, 118);

    --ninja-selected-background: rgb(248, 249, 251);

    --ninja-icon-color: var(--ninja-secondary-text-color);
    --ninja-icon-size: 1.2em;
    --ninja-separate-border: 1px solid var(--ninja-secondary-background-color);

    --ninja-modal-background: #fff;
    --ninja-modal-shadow: rgb(0 0 0 / 50%) 0px 16px 70px;

    --ninja-actions-height: 300px;
    --ninja-group-text-color: rgb(144, 149, 157);

    --ninja-footer-background: rgba(242, 242, 242, 0.4);

    --ninja-placeholder-color: #8e8e8e;

    font-size: var(--ninja-font-size);

    --ninja-z-index: 1;
  }

  :host(.dark) {
    --ninja-backdrop-filter: none;
    --ninja-overflow-background: rgba(0, 0, 0, 0.7);
    --ninja-text-color: #7d7d7d;

    --ninja-modal-background: rgba(17, 17, 17, 0.85);
    --ninja-accent-color: rgb(110, 94, 210);
    --ninja-secondary-background-color: rgba(51, 51, 51, 0.44);
    --ninja-secondary-text-color: #888;

    --ninja-selected-text-color: #eaeaea;
    --ninja-selected-background: rgba(51, 51, 51, 0.44);

    --ninja-icon-color: var(--ninja-secondary-text-color);
    --ninja-separate-border: 1px solid var(--ninja-secondary-background-color);

    --ninja-modal-shadow: 0 16px 70px rgba(0, 0, 0, 0.2);

    --ninja-group-text-color: rgb(144, 149, 157);

    --ninja-footer-background: rgba(30, 30, 30, 85%);
  }

  .modal {
    display: none;
    position: fixed;
    z-index: var(--ninja-z-index);
    left: 0;
    top: 0;
    width: 100%;
    height: 100%;
    overflow: auto;
    background: var(--ninja-overflow-background);
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
    -webkit-backdrop-filter: var(--ninja-backdrop-filter);
    backdrop-filter: var(--ninja-backdrop-filter);
    text-align: left;
    color: var(--ninja-text-color);
    font-family: var(--ninja-font-family);
  }
  .modal.visible {
    display: block;
  }

  .modal-content {
    position: relative;
    top: var(--ninja-top);
    margin: auto;
    padding: 0;
    display: flex;
    flex-direction: column;
    flex-shrink: 1;
    -webkit-box-flex: 1;
    flex-grow: 1;
    min-width: 0px;
    will-change: transform;
    background: var(--ninja-modal-background);
    border-radius: 0.5em;
    box-shadow: var(--ninja-modal-shadow);
    max-width: var(--ninja-width);
    overflow: hidden;
  }

  .bump {
    animation: zoom-in-zoom-out 0.2s ease;
  }

  @keyframes zoom-in-zoom-out {
    0% {
      transform: scale(0.99);
    }
    50% {
      transform: scale(1.01, 1.01);
    }
    100% {
      transform: scale(1, 1);
    }
  }

  .ninja-github {
    color: var(--ninja-keys-text-color);
    font-weight: normal;
    text-decoration: none;
  }

  .actions-list {
    max-height: var(--ninja-actions-height);
    overflow: auto;
    scroll-behavior: smooth;
    position: relative;
    margin: 0;
    padding: 0.5em 0;
    list-style: none;
    scroll-behavior: smooth;
  }

  .group-header {
    height: 1.375em;
    line-height: 1.375em;
    padding-left: 1.25em;
    padding-top: 0.5em;
    text-overflow: ellipsis;
    white-space: nowrap;
    overflow: hidden;
    font-size: 0.75em;
    line-height: 1em;
    color: var(--ninja-group-text-color);
    margin: 1px 0;
  }

  .modal-footer {
    background: var(--ninja-footer-background);
    padding: 0.5em 1em;
    display: flex;
    /* font-size: 0.75em; */
    border-top: var(--ninja-separate-border);
    color: var(--ninja-secondary-text-color);
  }

  .modal-footer .help {
    display: flex;
    margin-right: 1em;
    align-items: center;
    font-size: 0.75em;
  }

  .ninja-examplekey {
    background: var(--ninja-secondary-background-color);
    padding: 0.06em 0.25em;
    border-radius: var(--ninja-key-border-radius);
    color: var(--ninja-secondary-text-color);
    width: 1em;
    height: 1em;
    margin-right: 0.5em;
    font-size: 1.25em;
    fill: currentColor;
  }
  .ninja-examplekey.esc {
    width: auto;
    height: auto;
    font-size: 1.1em;
  }
  .ninja-examplekey.backspace {
    opacity: 0.7;
  }
`;

  // ../../../../../../../../Dropbox/projects/hazel/node_modules/ninja-keys/dist/ninja-keys.js
  var __decorate4 = function(decorators, target, key, desc) {
    var c6 = arguments.length, r6 = c6 < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d3;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r6 = Reflect.decorate(decorators, target, key, desc);
    else for (var i5 = decorators.length - 1; i5 >= 0; i5--) if (d3 = decorators[i5]) r6 = (c6 < 3 ? d3(r6) : c6 > 3 ? d3(target, key, r6) : d3(target, key)) || r6;
    return c6 > 3 && r6 && Object.defineProperty(target, key, r6), r6;
  };
  var NinjaKeys = class NinjaKeys2 extends s4 {
    constructor() {
      super(...arguments);
      this.placeholder = "Type a command or search...";
      this.disableHotkeys = false;
      this.hideBreadcrumbs = false;
      this.openHotkey = "cmd+k,ctrl+k";
      this.navigationUpHotkey = "up,shift+tab";
      this.navigationDownHotkey = "down,tab";
      this.closeHotkey = "esc";
      this.goBackHotkey = "backspace";
      this.selectHotkey = "enter";
      this.hotKeysJoinedView = false;
      this.noAutoLoadMdIcons = false;
      this.data = [];
      this.visible = false;
      this._bump = true;
      this._actionMatches = [];
      this._search = "";
      this._flatData = [];
      this._headerRef = e9();
    }
    /**
     * Public methods
     */
    /**
     * Show a modal
     */
    open(options = {}) {
      this._bump = true;
      this.visible = true;
      this._headerRef.value.focusSearch();
      if (this._actionMatches.length > 0) {
        this._selected = this._actionMatches[0];
      }
      this.setParent(options.parent);
    }
    /**
     * Close modal
     */
    close() {
      this._bump = false;
      this.visible = false;
    }
    /**
     * Navigate to group of actions
     * @param parent id of parent group/action
     */
    setParent(parent) {
      if (!parent) {
        this._currentRoot = void 0;
      } else {
        this._currentRoot = parent;
      }
      this._selected = void 0;
      this._search = "";
      this._headerRef.value.setSearch("");
    }
    get breadcrumbs() {
      var _a;
      const path = [];
      let parentAction = (_a = this._selected) === null || _a === void 0 ? void 0 : _a.parent;
      if (parentAction) {
        path.push(parentAction);
        while (parentAction) {
          const action = this._flatData.find((a4) => a4.id === parentAction);
          if (action === null || action === void 0 ? void 0 : action.parent) {
            path.push(action.parent);
          }
          parentAction = action ? action.parent : void 0;
        }
      }
      return path.reverse();
    }
    connectedCallback() {
      super.connectedCallback();
      if (!this.noAutoLoadMdIcons) {
        document.fonts.load("24px Material Icons", "apps").then(() => {
        });
      }
      this._registerInternalHotkeys();
    }
    disconnectedCallback() {
      super.disconnectedCallback();
      this._unregisterInternalHotkeys();
    }
    _flattern(members, parent) {
      let children = [];
      if (!members) {
        members = [];
      }
      return members.map((mem) => {
        const alreadyFlatternByUser = mem.children && mem.children.some((value) => {
          return typeof value == "string";
        });
        const m3 = { ...mem, parent: mem.parent || parent };
        if (alreadyFlatternByUser) {
          return m3;
        } else {
          if (m3.children && m3.children.length) {
            parent = mem.id;
            children = [...children, ...m3.children];
          }
          m3.children = m3.children ? m3.children.map((c6) => c6.id) : [];
          return m3;
        }
      }).concat(children.length ? this._flattern(children, parent) : children);
    }
    update(changedProperties) {
      if (changedProperties.has("data") && !this.disableHotkeys) {
        this._flatData = this._flattern(this.data);
        this._flatData.filter((action) => !!action.hotkey).forEach((action) => {
          hotkeys_esm_default(action.hotkey, (event) => {
            event.preventDefault();
            if (action.handler) {
              action.handler(action);
            }
          });
        });
      }
      super.update(changedProperties);
    }
    _registerInternalHotkeys() {
      if (this.openHotkey) {
        hotkeys_esm_default(this.openHotkey, (event) => {
          event.preventDefault();
          this.visible ? this.close() : this.open();
        });
      }
      if (this.selectHotkey) {
        hotkeys_esm_default(this.selectHotkey, (event) => {
          if (!this.visible) {
            return;
          }
          event.preventDefault();
          this._actionSelected(this._actionMatches[this._selectedIndex]);
        });
      }
      if (this.goBackHotkey) {
        hotkeys_esm_default(this.goBackHotkey, (event) => {
          if (!this.visible) {
            return;
          }
          if (!this._search) {
            event.preventDefault();
            this._goBack();
          }
        });
      }
      if (this.navigationDownHotkey) {
        hotkeys_esm_default(this.navigationDownHotkey, (event) => {
          if (!this.visible) {
            return;
          }
          event.preventDefault();
          if (this._selectedIndex >= this._actionMatches.length - 1) {
            this._selected = this._actionMatches[0];
          } else {
            this._selected = this._actionMatches[this._selectedIndex + 1];
          }
        });
      }
      if (this.navigationUpHotkey) {
        hotkeys_esm_default(this.navigationUpHotkey, (event) => {
          if (!this.visible) {
            return;
          }
          event.preventDefault();
          if (this._selectedIndex === 0) {
            this._selected = this._actionMatches[this._actionMatches.length - 1];
          } else {
            this._selected = this._actionMatches[this._selectedIndex - 1];
          }
        });
      }
      if (this.closeHotkey) {
        hotkeys_esm_default(this.closeHotkey, () => {
          if (!this.visible) {
            return;
          }
          this.close();
        });
      }
    }
    _unregisterInternalHotkeys() {
      if (this.openHotkey) {
        hotkeys_esm_default.unbind(this.openHotkey);
      }
      if (this.selectHotkey) {
        hotkeys_esm_default.unbind(this.selectHotkey);
      }
      if (this.goBackHotkey) {
        hotkeys_esm_default.unbind(this.goBackHotkey);
      }
      if (this.navigationDownHotkey) {
        hotkeys_esm_default.unbind(this.navigationDownHotkey);
      }
      if (this.navigationUpHotkey) {
        hotkeys_esm_default.unbind(this.navigationUpHotkey);
      }
      if (this.closeHotkey) {
        hotkeys_esm_default.unbind(this.closeHotkey);
      }
    }
    _actionFocused(index, $event) {
      this._selected = index;
      $event.target.ensureInView();
    }
    _onTransitionEnd() {
      this._bump = false;
    }
    _goBack() {
      const parent = this.breadcrumbs.length > 1 ? this.breadcrumbs[this.breadcrumbs.length - 2] : void 0;
      this.setParent(parent);
    }
    render() {
      const classes = {
        bump: this._bump,
        "modal-content": true
      };
      const menuClasses = {
        visible: this.visible,
        modal: true
      };
      const actionMatches = this._flatData.filter((action) => {
        var _a;
        const regex = new RegExp(this._search, "gi");
        const matcher = action.title.match(regex) || ((_a = action.keywords) === null || _a === void 0 ? void 0 : _a.match(regex));
        if (!this._currentRoot && this._search) {
          return matcher;
        }
        return action.parent === this._currentRoot && matcher;
      });
      const sections = actionMatches.reduce((entryMap, e11) => entryMap.set(e11.section, [...entryMap.get(e11.section) || [], e11]), /* @__PURE__ */ new Map());
      this._actionMatches = [...sections.values()].flat();
      if (this._actionMatches.length > 0 && this._selectedIndex === -1) {
        this._selected = this._actionMatches[0];
      }
      if (this._actionMatches.length === 0) {
        this._selected = void 0;
      }
      const actionsList = (actions) => x` ${c4(actions, (action) => action.id, (action) => {
        var _a;
        return x`<ninja-action
            exportparts="ninja-action,ninja-selected,ninja-icon"
            .selected=${l6(action.id === ((_a = this._selected) === null || _a === void 0 ? void 0 : _a.id))}
            .hotKeysJoinedView=${this.hotKeysJoinedView}
            @mouseover=${(event) => this._actionFocused(action, event)}
            @actionsSelected=${(event) => this._actionSelected(event.detail)}
            .action=${action}
          ></ninja-action>`;
      })}`;
      const itemTemplates = [];
      sections.forEach((actions, section) => {
        const header = section ? x`<div class="group-header">${section}</div>` : void 0;
        itemTemplates.push(x`${header}${actionsList(actions)}`);
      });
      return x`
      <div @click=${this._overlayClick} class=${o9(menuClasses)}>
        <div class=${o9(classes)} @animationend=${this._onTransitionEnd}>
          <ninja-header
            exportparts="ninja-input,ninja-input-wrapper"
            ${n8(this._headerRef)}
            .placeholder=${this.placeholder}
            .hideBreadcrumbs=${this.hideBreadcrumbs}
            .breadcrumbs=${this.breadcrumbs}
            @change=${this._handleInput}
            @setParent=${(event) => this.setParent(event.detail.parent)}
            @close=${this.close}
          >
          </ninja-header>
          <div class="modal-body">
            <div class="actions-list" part="actions-list">${itemTemplates}</div>
          </div>
          <slot name="footer"> ${footerHtml} </slot>
        </div>
      </div>
    `;
    }
    get _selectedIndex() {
      if (!this._selected) {
        return -1;
      }
      return this._actionMatches.indexOf(this._selected);
    }
    _actionSelected(action) {
      var _a;
      this.dispatchEvent(new CustomEvent("selected", {
        detail: { search: this._search, action },
        bubbles: true,
        composed: true
      }));
      if (!action) {
        return;
      }
      if (action.children && ((_a = action.children) === null || _a === void 0 ? void 0 : _a.length) > 0) {
        this._currentRoot = action.id;
        this._search = "";
      }
      this._headerRef.value.setSearch("");
      this._headerRef.value.focusSearch();
      if (action.handler) {
        const result = action.handler(action);
        if (!(result === null || result === void 0 ? void 0 : result.keepOpen)) {
          this.close();
        }
      }
      this._bump = true;
    }
    async _handleInput(event) {
      this._search = event.detail.search;
      await this.updateComplete;
      this.dispatchEvent(new CustomEvent("change", {
        detail: { search: this._search, actions: this._actionMatches },
        bubbles: true,
        composed: true
      }));
    }
    _overlayClick(event) {
      var _a;
      if ((_a = event.target) === null || _a === void 0 ? void 0 : _a.classList.contains("modal")) {
        this.close();
      }
    }
  };
  NinjaKeys.styles = [baseStyles];
  __decorate4([
    n5({ type: String })
  ], NinjaKeys.prototype, "placeholder", void 0);
  __decorate4([
    n5({ type: Boolean })
  ], NinjaKeys.prototype, "disableHotkeys", void 0);
  __decorate4([
    n5({ type: Boolean })
  ], NinjaKeys.prototype, "hideBreadcrumbs", void 0);
  __decorate4([
    n5()
  ], NinjaKeys.prototype, "openHotkey", void 0);
  __decorate4([
    n5()
  ], NinjaKeys.prototype, "navigationUpHotkey", void 0);
  __decorate4([
    n5()
  ], NinjaKeys.prototype, "navigationDownHotkey", void 0);
  __decorate4([
    n5()
  ], NinjaKeys.prototype, "closeHotkey", void 0);
  __decorate4([
    n5()
  ], NinjaKeys.prototype, "goBackHotkey", void 0);
  __decorate4([
    n5()
  ], NinjaKeys.prototype, "selectHotkey", void 0);
  __decorate4([
    n5({ type: Boolean })
  ], NinjaKeys.prototype, "hotKeysJoinedView", void 0);
  __decorate4([
    n5({ type: Boolean })
  ], NinjaKeys.prototype, "noAutoLoadMdIcons", void 0);
  __decorate4([
    n5({
      type: Array,
      hasChanged() {
        return true;
      }
    })
  ], NinjaKeys.prototype, "data", void 0);
  __decorate4([
    t3()
  ], NinjaKeys.prototype, "visible", void 0);
  __decorate4([
    t3()
  ], NinjaKeys.prototype, "_bump", void 0);
  __decorate4([
    t3()
  ], NinjaKeys.prototype, "_actionMatches", void 0);
  __decorate4([
    t3()
  ], NinjaKeys.prototype, "_search", void 0);
  __decorate4([
    t3()
  ], NinjaKeys.prototype, "_currentRoot", void 0);
  __decorate4([
    t3()
  ], NinjaKeys.prototype, "_flatData", void 0);
  __decorate4([
    t3()
  ], NinjaKeys.prototype, "breadcrumbs", null);
  __decorate4([
    t3()
  ], NinjaKeys.prototype, "_selected", void 0);
  NinjaKeys = __decorate4([
    e4("ninja-keys")
  ], NinjaKeys);

  // prebundle.js
  var import_algebrite = __toESM(require_algebrite());
  window.Algebrite = import_algebrite.default;
  hotkeys_esm_default.filter = (event) => {
    const path = typeof event.composedPath === "function" ? event.composedPath() : [];
    const target = event.target || event.srcElement;
    const { tagName, id } = target;
    if (id == "clipboard-shim") {
      return true;
    }
    const inNinjaKeys = path.some((el) => el && el.tagName === "NINJA-KEYS");
    if (inNinjaKeys) {
      return ["Escape", "Enter", "ArrowUp", "ArrowDown", "Backspace", "Tab"].includes(event.key);
    }
    let flag2 = true;
    const isInput = tagName === "INPUT" && !["checkbox", "radio", "range", "button", "file", "reset", "submit", "color"].includes(target.type);
    if (target.isContentEditable || (isInput || tagName === "TEXTAREA" || tagName === "SELECT") && !target.readOnly) {
      flag2 = false;
    }
    return flag2;
  };
})();
/*! Bundled license information:

@lit/reactive-element/css-tag.js:
  (**
   * @license
   * Copyright 2019 Google LLC
   * SPDX-License-Identifier: BSD-3-Clause
   *)

@lit/reactive-element/reactive-element.js:
  (**
   * @license
   * Copyright 2017 Google LLC
   * SPDX-License-Identifier: BSD-3-Clause
   *)

lit-html/lit-html.js:
  (**
   * @license
   * Copyright 2017 Google LLC
   * SPDX-License-Identifier: BSD-3-Clause
   *)

lit-element/lit-element.js:
  (**
   * @license
   * Copyright 2017 Google LLC
   * SPDX-License-Identifier: BSD-3-Clause
   *)

@lit/reactive-element/decorators/custom-element.js:
  (**
   * @license
   * Copyright 2017 Google LLC
   * SPDX-License-Identifier: BSD-3-Clause
   *)

@lit/reactive-element/decorators/property.js:
  (**
   * @license
   * Copyright 2017 Google LLC
   * SPDX-License-Identifier: BSD-3-Clause
   *)

@lit/reactive-element/decorators/state.js:
  (**
   * @license
   * Copyright 2017 Google LLC
   * SPDX-License-Identifier: BSD-3-Clause
   *)

@lit/reactive-element/decorators/base.js:
  (**
   * @license
   * Copyright 2017 Google LLC
   * SPDX-License-Identifier: BSD-3-Clause
   *)

@lit/reactive-element/decorators/event-options.js:
  (**
   * @license
   * Copyright 2017 Google LLC
   * SPDX-License-Identifier: BSD-3-Clause
   *)

@lit/reactive-element/decorators/query.js:
  (**
   * @license
   * Copyright 2017 Google LLC
   * SPDX-License-Identifier: BSD-3-Clause
   *)

@lit/reactive-element/decorators/query-all.js:
  (**
   * @license
   * Copyright 2017 Google LLC
   * SPDX-License-Identifier: BSD-3-Clause
   *)

@lit/reactive-element/decorators/query-async.js:
  (**
   * @license
   * Copyright 2017 Google LLC
   * SPDX-License-Identifier: BSD-3-Clause
   *)

@lit/reactive-element/decorators/query-assigned-elements.js:
  (**
   * @license
   * Copyright 2021 Google LLC
   * SPDX-License-Identifier: BSD-3-Clause
   *)

@lit/reactive-element/decorators/query-assigned-nodes.js:
  (**
   * @license
   * Copyright 2017 Google LLC
   * SPDX-License-Identifier: BSD-3-Clause
   *)

lit-html/directive.js:
  (**
   * @license
   * Copyright 2017 Google LLC
   * SPDX-License-Identifier: BSD-3-Clause
   *)

lit-html/directive-helpers.js:
  (**
   * @license
   * Copyright 2020 Google LLC
   * SPDX-License-Identifier: BSD-3-Clause
   *)

lit-html/directives/repeat.js:
  (**
   * @license
   * Copyright 2017 Google LLC
   * SPDX-License-Identifier: BSD-3-Clause
   *)

lit-html/directives/live.js:
  (**
   * @license
   * Copyright 2020 Google LLC
   * SPDX-License-Identifier: BSD-3-Clause
   *)

lit-html/async-directive.js:
  (**
   * @license
   * Copyright 2017 Google LLC
   * SPDX-License-Identifier: BSD-3-Clause
   *)

lit-html/directives/ref.js:
  (**
   * @license
   * Copyright 2020 Google LLC
   * SPDX-License-Identifier: BSD-3-Clause
   *)

lit-html/directives/class-map.js:
  (**
   * @license
   * Copyright 2018 Google LLC
   * SPDX-License-Identifier: BSD-3-Clause
   *)

hotkeys-js/dist/hotkeys.esm.js:
  (*!
   * hotkeys-js v3.8.7
   * A simple micro-library for defining and dispatching keyboard shortcuts. It has no dependencies.
   * 
   * Copyright (c) 2021 kenny wong <wowohoo@qq.com>
   * http://jaywcjlove.github.io/hotkeys
   * 
   * Licensed under the MIT license.
   *)

lit-html/directives/unsafe-html.js:
  (**
   * @license
   * Copyright 2017 Google LLC
   * SPDX-License-Identifier: BSD-3-Clause
   *)

lit-html/directives/join.js:
  (**
   * @license
   * Copyright 2021 Google LLC
   * SPDX-License-Identifier: BSD-3-Clause
   *)

@material/mwc-icon/mwc-icon-host.css.js:
  (**
   * @license
   * Copyright 2021 Google LLC
   * SPDX-LIcense-Identifier: Apache-2.0
   *)

@material/mwc-icon/mwc-icon.js:
  (**
   * @license
   * Copyright 2018 Google LLC
   * SPDX-License-Identifier: Apache-2.0
   *)
*/
