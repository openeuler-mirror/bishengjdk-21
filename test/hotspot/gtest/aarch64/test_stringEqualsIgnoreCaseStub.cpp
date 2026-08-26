/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

#if defined(AARCH64) && !defined(ZERO)

#include "asm/macroAssembler.hpp"
#include "code/codeBlob.hpp"
#include "runtime/stubCodeGenerator.hpp"
#include "runtime/stubRoutines.hpp"
#include "runtime/vm_version.hpp"
#include "unittest.hpp"
#include "utilities/bytes.hpp"

#define __ _masm.

namespace {

using StubFn = jint (*)(const jbyte*, const jbyte*, jint);
using ProbeFn = jint (*)(const jbyte*, const jbyte*, jint, uint32_t*);

constexpr jint ci_match = -1;
constexpr jint ci_mismatch = -2;
constexpr int max_vector_bytes = FloatRegister::sve_vl_max;

class RegisterPreservationProbe {
 private:
  static constexpr int frame_size = 176;
  static constexpr int failure_pointer_offset = 160;

  BufferBlob* _blob;
  address _entry;

 public:
  RegisterPreservationProbe(address target)
      : _blob(BufferBlob::create("equalsIgnoreCase register probe", 4096)),
        _entry(nullptr) {
    if (_blob == nullptr) {
      return;
    }

    CodeBuffer code(_blob);
    MacroAssembler _masm(&code);
    _entry = __ pc();

    const Register preserved_gprs[] = {
        r19, r20, r21, r22, r23, r24, r25, r26, r27, r28
    };
    const uint64_t gpr_sentinels[] = {
        0x1919191919191919ULL, 0x2020202020202020ULL,
        0x2121212121212121ULL, 0x2222222222222222ULL,
        0x2323232323232323ULL, 0x2424242424242424ULL,
        0x2525252525252525ULL, 0x2626262626262626ULL,
        0x2727272727272727ULL, 0x2828282828282828ULL
    };
    const FloatRegister preserved_fprs[] = {
        v8, v9, v10, v11, v12, v13, v14, v15
    };
    const uint64_t fpr_sentinels[] = {
        0x0808080808080808ULL, 0x0909090909090909ULL,
        0x1010101010101010ULL, 0x1111111111111111ULL,
        0x1212121212121212ULL, 0x1313131313131313ULL,
        0x1414141414141414ULL, 0x1515151515151515ULL
    };
    STATIC_ASSERT(ARRAY_SIZE(preserved_gprs) == ARRAY_SIZE(gpr_sentinels));
    STATIC_ASSERT(ARRAY_SIZE(preserved_fprs) == ARRAY_SIZE(fpr_sentinels));

    __ sub(sp, sp, frame_size);
    __ stp(r29, lr, Address(sp, 0));
    __ stp(r19, r20, Address(sp, 16));
    __ stp(r21, r22, Address(sp, 32));
    __ stp(r23, r24, Address(sp, 48));
    __ stp(r25, r26, Address(sp, 64));
    __ stp(r27, r28, Address(sp, 80));
    __ stpd(v8, v9, Address(sp, 96));
    __ stpd(v10, v11, Address(sp, 112));
    __ stpd(v12, v13, Address(sp, 128));
    __ stpd(v14, v15, Address(sp, 144));
    __ str(r3, Address(sp, failure_pointer_offset));

    for (unsigned int i = 0; i < ARRAY_SIZE(preserved_gprs); i++) {
      __ mov(preserved_gprs[i], gpr_sentinels[i]);
    }
    for (unsigned int i = 0; i < ARRAY_SIZE(preserved_fprs); i++) {
      __ mov(r14, fpr_sentinels[i]);
      __ fmovd(preserved_fprs[i], r14);
    }
    __ sve_ptrue(p7, Assembler::B);

    __ mov(rscratch1, reinterpret_cast<uint64_t>(target));
    __ blr(rscratch1);

    __ movw(r14, r0);
    __ movw(r15, 0);
    for (unsigned int i = 0; i < ARRAY_SIZE(preserved_gprs); i++) {
      __ mov(r16, gpr_sentinels[i]);
      __ cmp(preserved_gprs[i], r16);
      __ cset(r17, Assembler::NE);
      if (i != 0) {
        __ lsl(r17, r17, i);
      }
      __ orr(r15, r15, r17);
    }
    for (unsigned int i = 0; i < ARRAY_SIZE(preserved_fprs); i++) {
      __ fmovd(r17, preserved_fprs[i]);
      __ mov(r16, fpr_sentinels[i]);
      __ cmp(r17, r16);
      __ cset(r17, Assembler::NE);
      __ lsl(r17, r17, ARRAY_SIZE(preserved_gprs) + i);
      __ orr(r15, r15, r17);
    }
    __ sve_ptrue(p0, Assembler::B);
    __ sve_eors(p1, p0, p0, p7);
    __ cset(r17, Assembler::NE);
    __ lsl(r17, r17,
           ARRAY_SIZE(preserved_gprs) + ARRAY_SIZE(preserved_fprs));
    __ orr(r15, r15, r17);
    __ sve_ptrue(p7, Assembler::B);

    __ ldr(r16, Address(sp, failure_pointer_offset));
    __ strw(r15, Address(r16));
    __ ldpd(v8, v9, Address(sp, 96));
    __ ldpd(v10, v11, Address(sp, 112));
    __ ldpd(v12, v13, Address(sp, 128));
    __ ldpd(v14, v15, Address(sp, 144));
    __ ldp(r19, r20, Address(sp, 16));
    __ ldp(r21, r22, Address(sp, 32));
    __ ldp(r23, r24, Address(sp, 48));
    __ ldp(r25, r26, Address(sp, 64));
    __ ldp(r27, r28, Address(sp, 80));
    __ ldp(r29, lr, Address(sp, 0));
    __ add(sp, sp, frame_size);
    __ movw(r0, r14);
    __ ret(lr);
    __ flush();
  }

  ~RegisterPreservationProbe() {
    if (_blob != nullptr) {
      BufferBlob::free(_blob);
    }
  }

  bool is_valid() const {
    return _entry != nullptr;
  }

  jint invoke(const jbyte* left, const jbyte* right,
              jint len, uint32_t* failures) const {
    return CAST_TO_FN_PTR(ProbeFn, _entry)(left, right, len, failures);
  }
};

static jint invoke(address entry, const jbyte* left,
                   const jbyte* right, jint len) {
  return CAST_TO_FN_PTR(StubFn, entry)(left, right, len);
}

static const jbyte* bytes(const jchar* value) {
  return reinterpret_cast<const jbyte*>(value);
}

static int count_opcode(const StubCodeDesc* desc,
                        uint32_t mask, uint32_t opcode) {
  int count = 0;
  for (address pc = desc->begin(); pc < desc->end();
       pc += sizeof(uint32_t)) {
    if ((Bytes::get_native_u4(pc) & mask) == opcode) {
      count++;
    }
  }
  return count;
}

struct StubShape {
  int whilelo;
  int byte_gather;
  int word_gather;
  int bext;
  int nmatch;
  int uqadd;
  bool has_neon_loop;
};

static StubShape stub_shape(const StubCodeDesc* desc) {
  constexpr uint32_t neon_orr_mask = 0xffe0fc00u;
  constexpr uint32_t neon_orr_opcode = 0x4ea01c00u;
  constexpr uint32_t sve_whilelo_mask = 0xff20fc10u;
  constexpr uint32_t sve_whilelo_opcode = 0x25201c00u;
  constexpr uint32_t sve_gather_mask = 0xffe0e000u;
  constexpr uint32_t sve_byte_gather_opcode = 0x84004000u;
  constexpr uint32_t sve_word_gather_opcode = 0x85204000u;
  constexpr uint32_t sve2_bext_mask = 0xff20fc00u;
  constexpr uint32_t sve2_bext_opcode = 0x4500b000u;
  constexpr uint32_t sve2_nmatch_mask = 0xff20e010u;
  constexpr uint32_t sve2_nmatch_opcode = 0x45208010u;
  constexpr uint32_t sve2_pred_uqadd_mask = 0xff3fe000u;
  constexpr uint32_t sve2_pred_uqadd_opcode = 0x44198000u;

  StubShape shape = {};
  shape.whilelo =
      count_opcode(desc, sve_whilelo_mask, sve_whilelo_opcode);
  shape.byte_gather =
      count_opcode(desc, sve_gather_mask, sve_byte_gather_opcode);
  shape.word_gather =
      count_opcode(desc, sve_gather_mask, sve_word_gather_opcode);
  shape.bext = count_opcode(desc, sve2_bext_mask, sve2_bext_opcode);
  shape.nmatch =
      count_opcode(desc, sve2_nmatch_mask, sve2_nmatch_opcode);
  shape.uqadd =
      count_opcode(desc, sve2_pred_uqadd_mask, sve2_pred_uqadd_opcode);
  shape.has_neon_loop =
      count_opcode(desc, neon_orr_mask, neon_orr_opcode) > 0;
  return shape;
}

static void expect_sve_shape(address entry, const char* family,
                             bool needs_gather) {
  SCOPED_TRACE(family);
  const StubCodeDesc* desc = StubCodeDesc::desc_for(entry);
  ASSERT_NE(nullptr, desc);
  ASSERT_EQ(desc->begin(), entry);
  ASSERT_GT(desc->size_in_bytes(), 0);
  ASSERT_EQ(0u, desc->size_in_bytes() % sizeof(uint32_t));

  const StubShape shape = stub_shape(desc);
  EXPECT_GT(shape.whilelo, 0);
  EXPECT_EQ(0, shape.bext);
  EXPECT_EQ(0, shape.nmatch);
  EXPECT_EQ(0, shape.uqadd);
  EXPECT_FALSE(shape.has_neon_loop);
  EXPECT_EQ(needs_gather, shape.byte_gather > 0);
  EXPECT_EQ(needs_gather, shape.word_gather > 0);
}

static bool expect_result(address entry, const jbyte* left,
                          const jbyte* right, jint len, jint expected,
                          const char* family, const char* scenario) {
  const jint actual = invoke(entry, left, right, len);
  if (actual != expected) {
    ADD_FAILURE() << family << " scenario=" << scenario
                  << " len=" << len << " expected=" << expected
                  << " actual=" << actual;
    return false;
  }
  return true;
}

static bool expect_preserved(const RegisterPreservationProbe& probe,
                             const jbyte* left, const jbyte* right,
                             jint len, jint expected,
                             const char* family, const char* scenario) {
  uint32_t failures = UINT32_MAX;
  const jint actual = probe.invoke(left, right, len, &failures);
  if (actual != expected || failures != 0) {
    ADD_FAILURE() << family << " register preservation scenario="
                  << scenario << " len=" << len
                  << " expected=" << expected << " actual=" << actual
                  << " failure_mask=" << failures;
    return false;
  }
  return true;
}

static void fill_ll_pair(int lane, jbyte& left, jbyte& right) {
  switch (lane & 3) {
    case 0:
      left = 'a';
      right = 'A';
      break;
    case 1:
      left = 'B';
      right = 'b';
      break;
    case 2:
      left = static_cast<jbyte>(0xc0);
      right = static_cast<jbyte>(0xe0);
      break;
    default:
      left = static_cast<jbyte>(0xd6);
      right = static_cast<jbyte>(0xf6);
      break;
  }
}

static void fill_lu_pair(int lane, jbyte& left, jchar& right) {
  switch (lane & 3) {
    case 0:
      left = 'a';
      right = 'A';
      break;
    case 1:
      left = 'B';
      right = 'b';
      break;
    case 2:
      left = static_cast<jbyte>(0xb5);
      right = 0x03bc;
      break;
    default:
      left = static_cast<jbyte>(0xc0);
      right = 0x00e0;
      break;
  }
}

static void fill_uu_pair(int lane, jchar& left, jchar& right) {
  switch (lane & 3) {
    case 0:
      left = 'a';
      right = 'A';
      break;
    case 1:
      left = 'B';
      right = 'b';
      break;
    case 2:
      left = 0x03b1;
      right = 0x0391;
      break;
    default:
      left = 0x212a;
      right = 'k';
      break;
  }
}

template <typename Left, typename Right>
static bool test_vector_tail(address entry,
                             const RegisterPreservationProbe& probe,
                             const char* family, int vector_lanes,
                             void (*fill_pair)(int, Left&, Right&)) {
  constexpr int max_len = max_vector_bytes + 1;
  Left left[max_len];
  Right right[max_len];
  const int len = vector_lanes + 1;
  if (vector_lanes <= 0 || len > max_len) {
    ADD_FAILURE() << family << " invalid vector lane count=" << vector_lanes;
    return false;
  }

  for (int lane = 0; lane < len; lane++) {
    fill_pair(lane, left[lane], right[lane]);
  }
  const jbyte* left_bytes = reinterpret_cast<const jbyte*>(left);
  const jbyte* right_bytes = reinterpret_cast<const jbyte*>(right);
  bool passed = expect_result(entry, left_bytes, right_bytes, len, ci_match,
                              family, "vector-tail-match");
  passed &= expect_preserved(probe, left_bytes, right_bytes, len, ci_match,
                             family, "vector-tail-match");

  const Right saved = right[len - 1];
  right[len - 1] = static_cast<Right>('0');
  passed &= expect_result(entry, left_bytes, right_bytes, len, ci_mismatch,
                          family, "vector-tail-mismatch");
  passed &= expect_preserved(probe, left_bytes, right_bytes, len, ci_mismatch,
                             family, "vector-tail-mismatch");
  right[len - 1] = saved;
  return passed;
}

static void encode_supplementary(uint32_t code_point,
                                 jchar& high, jchar& low) {
  assert(code_point >= 0x10000 && code_point <= 0x10ffff,
         "supplementary scalar only");
  const uint32_t value = code_point - 0x10000;
  high = static_cast<jchar>(0xd800 + (value >> 10));
  low = static_cast<jchar>(0xdc00 + (value & 0x3ff));
}

static bool test_supplementary_and_checkpoint(
    address entry, const RegisterPreservationProbe& probe,
    int utf16_lanes) {
  constexpr int max_utf16_lanes =
      max_vector_bytes / static_cast<int>(sizeof(jchar));
  constexpr int max_len = 2 * max_utf16_lanes + 2;
  const int len = 2 * utf16_lanes + 2;
  const int position = utf16_lanes;
  jchar left[max_len];
  jchar right[max_len];
  if (utf16_lanes <= 1 || utf16_lanes > max_utf16_lanes || len > max_len) {
    ADD_FAILURE() << "UU invalid vector lane count=" << utf16_lanes;
    return false;
  }

  for (int lane = 0; lane < len; lane++) {
    fill_uu_pair(lane, left[lane], right[lane]);
  }
  encode_supplementary(0x10400, left[position], left[position + 1]);
  encode_supplementary(0x10428, right[position], right[position + 1]);
  bool passed = expect_result(entry, bytes(left), bytes(right), len, ci_match,
                              "UU", "supplementary-match");
  passed &= expect_preserved(probe, bytes(left), bytes(right), len, ci_match,
                             "UU", "supplementary-match");

  encode_supplementary(0x1f642, right[position], right[position + 1]);
  passed &= expect_result(entry, bytes(left), bytes(right), len, ci_mismatch,
                          "UU", "supplementary-mismatch");
  passed &= expect_preserved(probe, bytes(left), bytes(right), len,
                             ci_mismatch, "UU", "supplementary-mismatch");

  for (int lane = 0; lane < len; lane++) {
    fill_uu_pair(lane, left[lane], right[lane]);
  }
  left[position] = 0xd801;
  right[position] = 0xd801;
  passed &= expect_result(entry, bytes(left), bytes(right), len, position,
                          "UU", "malformed-checkpoint");
  passed &= expect_preserved(probe, bytes(left), bytes(right), len, position,
                             "UU", "malformed-checkpoint");
  return passed;
}

}  // namespace

TEST_VM(StringEqualsIgnoreCaseStub, direct_stub_abi) {
  address ll = StubRoutines::string_equals_ignore_case_ll();
  address lu = StubRoutines::string_equals_ignore_case_lu();
  address uu = StubRoutines::string_equals_ignore_case_uu();

  ASSERT_TRUE(UseStringEqualsIgnoreCaseIntrinsic);
  ASSERT_EQ(StringEqualsIgnoreCaseIntrinsicMinLength,
            StubRoutines::string_equals_ignore_case_min_length());
  const int vector_bytes = VM_Version::get_initial_sve_vector_length();
  ASSERT_GT(vector_bytes, 0);
  ASSERT_EQ(0, vector_bytes % static_cast<int>(sizeof(jchar)));
  const int ll_lanes = vector_bytes;
  const int utf16_lanes =
      vector_bytes / static_cast<int>(sizeof(jchar));

  ASSERT_NE(nullptr, ll);
  ASSERT_NE(nullptr, lu);
  ASSERT_NE(nullptr, uu);

  RegisterPreservationProbe ll_probe(ll);
  RegisterPreservationProbe lu_probe(lu);
  RegisterPreservationProbe uu_probe(uu);
  ASSERT_TRUE(ll_probe.is_valid());
  ASSERT_TRUE(lu_probe.is_valid());
  ASSERT_TRUE(uu_probe.is_valid());

  MACOS_AARCH64_ONLY(os::current_thread_enable_wx(WXExec));
  bool passed = true;
  passed &= test_vector_tail<jbyte, jbyte>(
      ll, ll_probe, "LL", ll_lanes, fill_ll_pair);
  passed &= test_vector_tail<jbyte, jchar>(
      lu, lu_probe, "LU", utf16_lanes, fill_lu_pair);
  passed &= test_vector_tail<jchar, jchar>(
      uu, uu_probe, "UU", utf16_lanes, fill_uu_pair);
  passed &= test_supplementary_and_checkpoint(uu, uu_probe, utf16_lanes);
  expect_sve_shape(ll, "LL", false);
  expect_sve_shape(lu, "LU", true);
  expect_sve_shape(uu, "UU", true);
  MACOS_AARCH64_ONLY(os::current_thread_enable_wx(WXWrite));
  EXPECT_TRUE(passed);
}

TEST_VM(StringEqualsIgnoreCaseStub, disabled_entries_are_null) {
  ASSERT_FALSE(UseStringEqualsIgnoreCaseIntrinsic);
  EXPECT_EQ(0, StubRoutines::string_equals_ignore_case_min_length());
  EXPECT_EQ(nullptr,
            StubRoutines::string_equals_ignore_case_ll());
  EXPECT_EQ(nullptr,
            StubRoutines::string_equals_ignore_case_lu());
  EXPECT_EQ(nullptr,
            StubRoutines::string_equals_ignore_case_uu());
}

#undef __

#endif  // defined(AARCH64) && !defined(ZERO)
