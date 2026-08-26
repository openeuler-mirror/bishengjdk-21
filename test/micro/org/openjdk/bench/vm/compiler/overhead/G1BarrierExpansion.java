/*
 * Copyright (c) 2026, Huawei Technologies Co., Ltd. All rights reserved.
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
 * Please contact Huawei Technologies Co., Ltd., Huawei Industrial Base,
 * Bantian, Longgang District, Shenzhen 518129, People's Republic of China,
 * or visit www.huawei.com if you need additional information or have any
 * questions.
 */

package org.openjdk.bench.vm.compiler.overhead;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.CompilerControl;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.State;

import java.util.concurrent.TimeUnit;

/**
 * Measures the C2 compilation cost of a method containing many G1 reference
 * store barriers. Run this benchmark with JDK images before and after late
 * barrier expansion and compare barrierStores_repeat_c2. Lower is better.
 *
 * The baseline benchmark measures the execution cost of the same workload.
 * RepeatCompilation deliberately recompiles only barrierStores, amplifying
 * compiler cost without changing the Java workload.
 */
@State(Scope.Benchmark)
@BenchmarkMode(Mode.SingleShotTime)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(value = 10, warmups = 1)
public class G1BarrierExpansion {
    private static final String REPEAT_BARRIER_STORES =
        "-XX:CompileCommand=option,org/openjdk/bench/vm/compiler/overhead/" +
        "G1BarrierExpansion.barrierStores,intx,RepeatCompilation,200";

    private final Holder holder = new Holder();
    private final Object value1 = new Object();
    private final Object value2 = new Object();

    @Benchmark
    @Fork(jvmArgs = {"-Xbatch", "-XX:-TieredCompilation", "-XX:+UseG1GC",
                     REPEAT_BARRIER_STORES})
    public Object barrierStores_repeat_c2() {
        return runBarrierStores();
    }

    @Benchmark
    @Fork(jvmArgs = {"-Xbatch", "-XX:-TieredCompilation", "-XX:+UseG1GC"})
    public Object barrierStores_baseline() {
        return runBarrierStores();
    }

    private Object runBarrierStores() {
        Object value = value1;
        for (int i = 0; i < 20_000; i++) {
            value = value == value1 ? value2 : value1;
            barrierStores(holder, value);
        }
        return holder.f63;
    }

    @CompilerControl(CompilerControl.Mode.DONT_INLINE)
    public static void barrierStores(Holder holder, Object value) {
        holder.f00 = value;
        holder.f01 = value;
        holder.f02 = value;
        holder.f03 = value;
        holder.f04 = value;
        holder.f05 = value;
        holder.f06 = value;
        holder.f07 = value;
        holder.f08 = value;
        holder.f09 = value;
        holder.f10 = value;
        holder.f11 = value;
        holder.f12 = value;
        holder.f13 = value;
        holder.f14 = value;
        holder.f15 = value;
        holder.f16 = value;
        holder.f17 = value;
        holder.f18 = value;
        holder.f19 = value;
        holder.f20 = value;
        holder.f21 = value;
        holder.f22 = value;
        holder.f23 = value;
        holder.f24 = value;
        holder.f25 = value;
        holder.f26 = value;
        holder.f27 = value;
        holder.f28 = value;
        holder.f29 = value;
        holder.f30 = value;
        holder.f31 = value;
        holder.f32 = value;
        holder.f33 = value;
        holder.f34 = value;
        holder.f35 = value;
        holder.f36 = value;
        holder.f37 = value;
        holder.f38 = value;
        holder.f39 = value;
        holder.f40 = value;
        holder.f41 = value;
        holder.f42 = value;
        holder.f43 = value;
        holder.f44 = value;
        holder.f45 = value;
        holder.f46 = value;
        holder.f47 = value;
        holder.f48 = value;
        holder.f49 = value;
        holder.f50 = value;
        holder.f51 = value;
        holder.f52 = value;
        holder.f53 = value;
        holder.f54 = value;
        holder.f55 = value;
        holder.f56 = value;
        holder.f57 = value;
        holder.f58 = value;
        holder.f59 = value;
        holder.f60 = value;
        holder.f61 = value;
        holder.f62 = value;
        holder.f63 = value;
    }

    static class Holder {
        Object f00;
        Object f01;
        Object f02;
        Object f03;
        Object f04;
        Object f05;
        Object f06;
        Object f07;
        Object f08;
        Object f09;
        Object f10;
        Object f11;
        Object f12;
        Object f13;
        Object f14;
        Object f15;
        Object f16;
        Object f17;
        Object f18;
        Object f19;
        Object f20;
        Object f21;
        Object f22;
        Object f23;
        Object f24;
        Object f25;
        Object f26;
        Object f27;
        Object f28;
        Object f29;
        Object f30;
        Object f31;
        Object f32;
        Object f33;
        Object f34;
        Object f35;
        Object f36;
        Object f37;
        Object f38;
        Object f39;
        Object f40;
        Object f41;
        Object f42;
        Object f43;
        Object f44;
        Object f45;
        Object f46;
        Object f47;
        Object f48;
        Object f49;
        Object f50;
        Object f51;
        Object f52;
        Object f53;
        Object f54;
        Object f55;
        Object f56;
        Object f57;
        Object f58;
        Object f59;
        Object f60;
        Object f61;
        Object f62;
        Object f63;
    }
}
