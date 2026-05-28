// Copyright (C) 2012-2013 Samplecount S.L.
// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#ifndef METHCLA_TYPES_H_INCLUDED
#define METHCLA_TYPES_H_INCLUDED

typedef enum Methcla_NodePlacement
{
    kMethcla_NodePlacementHeadOfGroup,
    kMethcla_NodePlacementTailOfGroup,
    kMethcla_NodePlacementBeforeNode,
    kMethcla_NodePlacementAfterNode
} Methcla_NodePlacement;

typedef enum Methcla_BusMappingFlags
{
    kMethcla_BusMappingInternal = 0x00,
    kMethcla_BusMappingExternal = 0x01,
    kMethcla_BusMappingFeedback = 0x02,
    kMethcla_BusMappingReplace = 0x04
} Methcla_BusMappingFlags;

typedef enum Methcla_NodeDoneFlags
{
    kMethcla_NodeDoneDoNothing = 0x00,
    kMethcla_NodeDoneFreeSelf = 0x01,
    kMethcla_NodeDoneFreePreceeding = 0x02,
    kMethcla_NodeDoneFreeFollowing = 0x04,
    kMethcla_NodeDoneFreeAllSiblings = 0x08,
    kMethcla_NodeDoneFreeParent = 0x10,
    kMethcla_NodeDoneNotify = 0x20
} Methcla_NodeDoneFlags;

#endif /* METHCLA_TYPES_H_INCLUDED */
