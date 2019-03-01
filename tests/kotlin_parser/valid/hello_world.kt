/*
 * Copyright 2010-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
 * that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.metadata.deserialization

import org.jetbrains.kotlin.metadata.ProtoBuf
import org.jetbrains.kotlin.protobuf.MessageLite

class VersionRequirementTable private constructor(private val infos: List<ProtoBuf.VersionRequirement>) {
    operator fun get(id: Int): ProtoBuf.VersionRequirement? = infos.getOrNull(id)

    companion object {
        val EMPTY = VersionRequirementTable(emptyList())

        fun create(table: ProtoBuf.VersionRequirementTable): VersionRequirementTable =
            if (table.requirementCount == 0) EMPTY else VersionRequirementTable(
                table.requirementList
            )
    }
}

class VersionRequirement(
    val version: Version,
    val kind: ProtoBuf.VersionRequirement.VersionKind,
    val level: DeprecationLevel,
    val errorCode: Int?,
    val message: String?
) {
    data class Version(val major: Int, val minor: Int, val patch: Int = 0) {
        fun asString(): String =
            if (patch == 0) "$major.$minor" else "$major.$minor.$patch"

        fun encode(
            writeVersion: (Int) -> Unit,
            writeVersionFull: (Int) -> Unit
        ) = when {
            this == INFINITY -> {
                // Do nothing: absence of version means INFINITY
            }
            major > MAJOR_MASK || minor > MINOR_MASK || patch > PATCH_MASK -> {
                writeVersionFull(major or (minor shl 8))
            }
            else -> {
                writeVersion(major or (minor shl MAJOR_BITS) or (patch shl (MAJOR_BITS + MINOR_BITS)))
            }
        }

        override fun toString(): String = asString()

    }


}