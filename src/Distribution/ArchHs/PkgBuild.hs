{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Copyright: (c) 2020-2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
-- Template of PKGBUILD file.
module Distribution.ArchHs.PkgBuild
  ( PkgBuild (..),
    mapLicense,
    showArchLicense,
    applyTemplate,
    felixTemplate,
  )
where

import Data.Text (Text, pack, unpack)
import Distribution.SPDX.LicenseId
import Lens.Micro ((&), (<&>))
import NeatInterpolation (text)
import qualified Web.ArchLinux.Types as Arch

-- | PkgBuild data type, representing needed information in filling the 'felixTemplate'.
data PkgBuild = PkgBuild
  { -- | Field @_hkgName@.
    _hkgName :: String,
    -- | Field @pkgname@
    _pkgName :: String,
    -- | Field @pkgver@
    _pkgVer :: String,
    -- | Field @pkgdesc@
    _pkgDesc :: String,
    -- | Field @url@
    _url :: String,
    -- | Field @license@
    _license :: String,
    -- | Array @depends@, which has been joined into 'String'
    _depends :: String,
    -- | Array @makedepends@, which has been joined into 'String'
    _makeDepends :: String,
    -- | Field @sha256sums@
    _sha256sums :: String,
    -- | License file name
    _licenseFile :: Maybe String,
    -- | Whether generate @prepare()@ bash function which calls @uusi@
    _enableUusi :: Bool,
    -- | List of dependencies whose version ranges will be removed in the @prepare()@ bash function using @uusi@
    _removeWithUusi :: [String],
    -- | Command-line flags
    _flags :: String
  }

-- | Map 'LicenseId' to 'ArchLicense'. License not provided by system will be mapped to @custom:...@.
mapLicense :: LicenseId -> Arch.License
mapLicense NullBSD = Arch.N_0BSD
mapLicense AAL = Arch.AAL
mapLicense Abstyles = Arch.Abstyles
mapLicense Adobe_2006 = Arch.Adobe_2006
mapLicense Adobe_Glyph = Arch.Adobe_Glyph
mapLicense ADSL = Arch.ADSL
mapLicense AFL_1_1 = Arch.AFL_1_1
mapLicense AFL_1_2 = Arch.AFL_1_2
mapLicense AFL_2_0 = Arch.AFL_2_0
mapLicense AFL_2_1 = Arch.AFL_2_1
mapLicense AFL_3_0 = Arch.AFL_3_0
mapLicense Afmparse = Arch.Afmparse
-- mapLicense AGPL_1_0 = Arch.AGPL_1_0
mapLicense AGPL_1_0_only = Arch.AGPL_1_0_only
mapLicense AGPL_1_0_or_later = Arch.AGPL_1_0_or_later
mapLicense AGPL_3_0_only = Arch.AGPL_3_0_only
mapLicense AGPL_3_0_or_later = Arch.AGPL_3_0_or_later
mapLicense Aladdin = Arch.Aladdin
mapLicense AMDPLPA = Arch.AMDPLPA
mapLicense AML = Arch.AML
mapLicense AMPAS = Arch.AMPAS
mapLicense ANTLR_PD_fallback = Arch.ANTLR_PD_fallback
mapLicense ANTLR_PD = Arch.ANTLR_PD
mapLicense Apache_1_0 = Arch.Apache_1_0
mapLicense Apache_1_1 = Arch.Apache_1_1
mapLicense Apache_2_0 = Arch.Apache_2_0
mapLicense APAFML = Arch.APAFML
mapLicense APL_1_0 = Arch.APL_1_0
mapLicense App_s2p = Arch.App_s2p
mapLicense APSL_1_0 = Arch.APSL_1_0
mapLicense APSL_1_1 = Arch.APSL_1_1
mapLicense APSL_1_2 = Arch.APSL_1_2
mapLicense APSL_2_0 = Arch.APSL_2_0
mapLicense Artistic_1_0_cl8 = Arch.Artistic_1_0_cl8
mapLicense Artistic_1_0_Perl = Arch.Artistic_1_0_Perl
mapLicense Artistic_1_0 = Arch.Artistic_1_0
mapLicense Artistic_2_0 = Arch.Artistic_2_0
mapLicense Bahyph = Arch.Bahyph
mapLicense Barr = Arch.Barr
mapLicense Beerware = Arch.Beerware
mapLicense BitTorrent_1_0 = Arch.BitTorrent_1_0
mapLicense BitTorrent_1_1 = Arch.BitTorrent_1_1
mapLicense Blessing = Arch.Blessing
mapLicense BlueOak_1_0_0 = Arch.BlueOak_1_0_0
mapLicense Borceux = Arch.Borceux
mapLicense BSD_1_Clause = Arch.BSD_1_Clause
mapLicense BSD_2_Clause_Patent = Arch.BSD_2_Clause_Patent
mapLicense BSD_2_Clause_Views = Arch.BSD_2_Clause_Views
mapLicense BSD_2_Clause = Arch.BSD_2_Clause
-- mapLicense BSD_2_Clause_FreeBSD = Arch.BSD_2_Clause_FreeBSD
-- mapLicense BSD_2_Clause_NetBSD = Arch.BSD_2_Clause_NetBSD
mapLicense BSD_3_Clause_Attribution = Arch.BSD_3_Clause_Attribution
mapLicense BSD_3_Clause_Clear = Arch.BSD_3_Clause_Clear
mapLicense BSD_3_Clause_LBNL = Arch.BSD_3_Clause_LBNL
mapLicense BSD_3_Clause_Modification = Arch.BSD_3_Clause_Modification
mapLicense BSD_3_Clause_No_Military_License = Arch.BSD_3_Clause_No_Military_License
mapLicense BSD_3_Clause_No_Nuclear_License_2014 = Arch.BSD_3_Clause_No_Nuclear_License_2014
mapLicense BSD_3_Clause_No_Nuclear_License = Arch.BSD_3_Clause_No_Nuclear_License
mapLicense BSD_3_Clause_No_Nuclear_Warranty = Arch.BSD_3_Clause_No_Nuclear_Warranty
mapLicense BSD_3_Clause_Open_MPI = Arch.BSD_3_Clause_Open_MPI
mapLicense BSD_3_Clause = Arch.BSD_3_Clause
mapLicense BSD_4_Clause_Shortened = Arch.BSD_4_Clause_Shortened
mapLicense BSD_4_Clause_UC = Arch.BSD_4_Clause_UC
mapLicense BSD_4_Clause = Arch.BSD_4_Clause
mapLicense BSD_Protection = Arch.BSD_Protection
mapLicense BSD_Source_Code = Arch.BSD_Source_Code
mapLicense BSL_1_0 = Arch.BSL_1_0
mapLicense BUSL_1_1 = Arch.BUSL_1_1
-- mapLicense Bzip2_1_0_5 = Arch.Bzip2_1_0_5
mapLicense Bzip2_1_0_6 = Arch.Bzip2_1_0_6
mapLicense C_UDA_1_0 = Arch.C_UDA_1_0
mapLicense CAL_1_0_Combined_Work_Exception = Arch.CAL_1_0_Combined_Work_Exception
mapLicense CAL_1_0 = Arch.CAL_1_0
mapLicense Caldera = Arch.Caldera
mapLicense CATOSL_1_1 = Arch.CATOSL_1_1
mapLicense CC_BY_1_0 = Arch.CC_BY_1_0
mapLicense CC_BY_2_0 = Arch.CC_BY_2_0
mapLicense CC_BY_2_5_AU = Arch.CC_BY_2_5_AU
mapLicense CC_BY_2_5 = Arch.CC_BY_2_5
mapLicense CC_BY_3_0_AT = Arch.CC_BY_3_0_AT
mapLicense CC_BY_3_0_DE = Arch.CC_BY_3_0_DE
mapLicense CC_BY_3_0_NL = Arch.CC_BY_3_0_NL
mapLicense CC_BY_3_0_US = Arch.CC_BY_3_0_US
mapLicense CC_BY_3_0 = Arch.CC_BY_3_0
mapLicense CC_BY_4_0 = Arch.CC_BY_4_0
mapLicense CC_BY_NC_1_0 = Arch.CC_BY_NC_1_0
mapLicense CC_BY_NC_2_0 = Arch.CC_BY_NC_2_0
mapLicense CC_BY_NC_2_5 = Arch.CC_BY_NC_2_5
mapLicense CC_BY_NC_3_0_DE = Arch.CC_BY_NC_3_0_DE
mapLicense CC_BY_NC_3_0 = Arch.CC_BY_NC_3_0
mapLicense CC_BY_NC_4_0 = Arch.CC_BY_NC_4_0
mapLicense CC_BY_NC_ND_1_0 = Arch.CC_BY_NC_ND_1_0
mapLicense CC_BY_NC_ND_2_0 = Arch.CC_BY_NC_ND_2_0
mapLicense CC_BY_NC_ND_2_5 = Arch.CC_BY_NC_ND_2_5
mapLicense CC_BY_NC_ND_3_0_DE = Arch.CC_BY_NC_ND_3_0_DE
mapLicense CC_BY_NC_ND_3_0_IGO = Arch.CC_BY_NC_ND_3_0_IGO
mapLicense CC_BY_NC_ND_3_0 = Arch.CC_BY_NC_ND_3_0
mapLicense CC_BY_NC_ND_4_0 = Arch.CC_BY_NC_ND_4_0
mapLicense CC_BY_NC_SA_1_0 = Arch.CC_BY_NC_SA_1_0
mapLicense CC_BY_NC_SA_2_0_FR = Arch.CC_BY_NC_SA_2_0_FR
mapLicense CC_BY_NC_SA_2_0_UK = Arch.CC_BY_NC_SA_2_0_UK
mapLicense CC_BY_NC_SA_2_0 = Arch.CC_BY_NC_SA_2_0
mapLicense CC_BY_NC_SA_2_5 = Arch.CC_BY_NC_SA_2_5
mapLicense CC_BY_NC_SA_3_0_DE = Arch.CC_BY_NC_SA_3_0_DE
mapLicense CC_BY_NC_SA_3_0_IGO = Arch.CC_BY_NC_SA_3_0_IGO
mapLicense CC_BY_NC_SA_3_0 = Arch.CC_BY_NC_SA_3_0
mapLicense CC_BY_NC_SA_4_0 = Arch.CC_BY_NC_SA_4_0
mapLicense CC_BY_ND_1_0 = Arch.CC_BY_ND_1_0
mapLicense CC_BY_ND_2_0 = Arch.CC_BY_ND_2_0
mapLicense CC_BY_ND_2_5 = Arch.CC_BY_ND_2_5
mapLicense CC_BY_ND_3_0_DE = Arch.CC_BY_ND_3_0_DE
mapLicense CC_BY_ND_3_0 = Arch.CC_BY_ND_3_0
mapLicense CC_BY_ND_4_0 = Arch.CC_BY_ND_4_0
mapLicense CC_BY_SA_1_0 = Arch.CC_BY_SA_1_0
mapLicense CC_BY_SA_2_0_UK = Arch.CC_BY_SA_2_0_UK
mapLicense CC_BY_SA_2_0 = Arch.CC_BY_SA_2_0
mapLicense CC_BY_SA_2_1_JP = Arch.CC_BY_SA_2_1_JP
mapLicense CC_BY_SA_2_5 = Arch.CC_BY_SA_2_5
mapLicense CC_BY_SA_3_0_AT = Arch.CC_BY_SA_3_0_AT
mapLicense CC_BY_SA_3_0_DE = Arch.CC_BY_SA_3_0_DE
mapLicense CC_BY_SA_3_0 = Arch.CC_BY_SA_3_0
mapLicense CC_BY_SA_4_0 = Arch.CC_BY_SA_4_0
mapLicense CC_PDDC = Arch.CC_PDDC
mapLicense CC0_1_0 = Arch.CC0_1_0
mapLicense CDDL_1_0 = Arch.CDDL_1_0
mapLicense CDDL_1_1 = Arch.CDDL_1_1
mapLicense CDL_1_0 = Arch.CDL_1_0
mapLicense CDLA_Permissive_1_0 = Arch.CDLA_Permissive_1_0
mapLicense CDLA_Permissive_2_0 = Arch.CDLA_Permissive_2_0
mapLicense CDLA_Sharing_1_0 = Arch.CDLA_Sharing_1_0
mapLicense CECILL_1_0 = Arch.CECILL_1_0
mapLicense CECILL_1_1 = Arch.CECILL_1_1
mapLicense CECILL_2_0 = Arch.CECILL_2_0
mapLicense CECILL_2_1 = Arch.CECILL_2_1
mapLicense CECILL_B = Arch.CECILL_B
mapLicense CECILL_C = Arch.CECILL_C
mapLicense CERN_OHL_1_1 = Arch.CERN_OHL_1_1
mapLicense CERN_OHL_1_2 = Arch.CERN_OHL_1_2
mapLicense CERN_OHL_P_2_0 = Arch.CERN_OHL_P_2_0
mapLicense CERN_OHL_S_2_0 = Arch.CERN_OHL_S_2_0
mapLicense CERN_OHL_W_2_0 = Arch.CERN_OHL_W_2_0
mapLicense ClArtistic = Arch.ClArtistic
mapLicense CNRI_Jython = Arch.CNRI_Jython
mapLicense CNRI_Python_GPL_Compatible = Arch.CNRI_Python_GPL_Compatible
mapLicense CNRI_Python = Arch.CNRI_Python
mapLicense COIL_1_0 = Arch.COIL_1_0
mapLicense Community_Spec_1_0 = Arch.Community_Spec_1_0
mapLicense Condor_1_1 = Arch.Condor_1_1
mapLicense Copyleft_next_0_3_0 = Arch.Copyleft_next_0_3_0
mapLicense Copyleft_next_0_3_1 = Arch.Copyleft_next_0_3_1
mapLicense CPAL_1_0 = Arch.CPAL_1_0
mapLicense CPL_1_0 = Arch.CPL_1_0
mapLicense CPOL_1_02 = Arch.CPOL_1_02
mapLicense Crossword = Arch.Crossword
mapLicense CrystalStacker = Arch.CrystalStacker
mapLicense CUA_OPL_1_0 = Arch.CUA_OPL_1_0
mapLicense Cube = Arch.Cube
mapLicense Curl = Arch.Curl
mapLicense D_FSL_1_0 = Arch.D_FSL_1_0
mapLicense Diffmark = Arch.Diffmark
mapLicense DL_DE_BY_2_0 = Arch.DL_DE_BY_2_0
mapLicense DOC = Arch.Doc
mapLicense Dotseqn = Arch.Dotseqn
mapLicense DRL_1_0 = Arch.DRL_1_0
mapLicense DSDP = Arch.DSDP
mapLicense Dvipdfm = Arch.Dvipdfm
mapLicense ECL_1_0 = Arch.ECL_1_0
mapLicense ECL_2_0 = Arch.ECL_2_0
mapLicense EFL_1_0 = Arch.EFL_1_0
mapLicense EFL_2_0 = Arch.EFL_2_0
mapLicense EGenix = Arch.EGenix
mapLicense Elastic_2_0 = Arch.Elastic_2_0
mapLicense Entessa = Arch.Entessa
mapLicense EPICS = Arch.EPICS
mapLicense EPL_1_0 = Arch.EPL_1_0
mapLicense EPL_2_0 = Arch.EPL_2_0
mapLicense ErlPL_1_1 = Arch.ErlPL_1_1
mapLicense Etalab_2_0 = Arch.Etalab_2_0
mapLicense EUDatagrid = Arch.EUDatagrid
mapLicense EUPL_1_0 = Arch.EUPL_1_0
mapLicense EUPL_1_1 = Arch.EUPL_1_1
mapLicense EUPL_1_2 = Arch.EUPL_1_2
mapLicense Eurosym = Arch.Eurosym
mapLicense Fair = Arch.Fair
mapLicense FDK_AAC = Arch.FDK_AAC
mapLicense Frameworx_1_0 = Arch.Frameworx_1_0
mapLicense FreeBSD_DOC = Arch.FreeBSD_DOC
mapLicense FreeImage = Arch.FreeImage
mapLicense FSFAP = Arch.FSFAP
mapLicense FSFULLR = Arch.FSFULLR
mapLicense FSFUL = Arch.FSFUL
mapLicense FTL = Arch.FTL
mapLicense GD = Arch.GD
mapLicense GFDL_1_1_invariants_only = Arch.GFDL_1_1_invariants_only
mapLicense GFDL_1_1_invariants_or_later = Arch.GFDL_1_1_invariants_or_later
mapLicense GFDL_1_1_no_invariants_only = Arch.GFDL_1_1_no_invariants_only
mapLicense GFDL_1_1_no_invariants_or_later = Arch.GFDL_1_1_no_invariants_or_later
mapLicense GFDL_1_1_only = Arch.GFDL_1_1_only
mapLicense GFDL_1_1_or_later = Arch.GFDL_1_1_or_later
mapLicense GFDL_1_2_invariants_only = Arch.GFDL_1_2_invariants_only
mapLicense GFDL_1_2_invariants_or_later = Arch.GFDL_1_2_invariants_or_later
mapLicense GFDL_1_2_no_invariants_only = Arch.GFDL_1_2_no_invariants_only
mapLicense GFDL_1_2_no_invariants_or_later = Arch.GFDL_1_2_no_invariants_or_later
mapLicense GFDL_1_2_only = Arch.GFDL_1_2_only
mapLicense GFDL_1_2_or_later = Arch.GFDL_1_2_or_later
mapLicense GFDL_1_3_invariants_only = Arch.GFDL_1_3_invariants_only
mapLicense GFDL_1_3_invariants_or_later = Arch.GFDL_1_3_invariants_or_later
mapLicense GFDL_1_3_no_invariants_only = Arch.GFDL_1_3_no_invariants_only
mapLicense GFDL_1_3_no_invariants_or_later = Arch.GFDL_1_3_no_invariants_or_later
mapLicense GFDL_1_3_only = Arch.GFDL_1_3_only
mapLicense GFDL_1_3_or_later = Arch.GFDL_1_3_or_later
mapLicense Giftware = Arch.Giftware
mapLicense GL2PS = Arch.GL2PS
mapLicense Glide = Arch.Glide
mapLicense Glulxe = Arch.Glulxe
mapLicense GLWTPL = Arch.GLWTPL
mapLicense Gnuplot = Arch.Gnuplot
mapLicense GPL_1_0_only = Arch.GPL_1_0_only
mapLicense GPL_1_0_or_later = Arch.GPL_1_0_or_later
mapLicense GPL_2_0_only = Arch.GPL_2_0_only
mapLicense GPL_2_0_or_later = Arch.GPL_2_0_or_later
mapLicense GPL_3_0_only = Arch.GPL_3_0_only
mapLicense GPL_3_0_or_later = Arch.GPL_3_0_or_later
mapLicense GSOAP_1_3b = Arch.GSOAP_1_3b
mapLicense HaskellReport = Arch.HaskellReport
mapLicense Hippocratic_2_1 = Arch.Hippocratic_2_1
mapLicense HPND_sell_variant = Arch.HPND_sell_variant
mapLicense HPND = Arch.HPND
mapLicense HTMLTIDY = Arch.HTMLTIDY
mapLicense IBM_pibs = Arch.IBM_pibs
mapLicense ICU = Arch.ICU
mapLicense IJG = Arch.IJG
mapLicense ImageMagick = Arch.ImageMagick
mapLicense IMatix = Arch.IMatix
mapLicense Imlib2 = Arch.Imlib2
mapLicense Info_ZIP = Arch.Info_ZIP
mapLicense Intel_ACPI = Arch.Intel_ACPI
mapLicense Intel = Arch.Intel
mapLicense Interbase_1_0 = Arch.Interbase_1_0
mapLicense IPA = Arch.IPA
mapLicense IPL_1_0 = Arch.IPL_1_0
mapLicense ISC = Arch.ISC
mapLicense Jam = Arch.Jam
mapLicense JasPer_2_0 = Arch.JasPer_2_0
mapLicense JPNIC = Arch.JPNIC
mapLicense JSON = Arch.JSON
mapLicense LAL_1_2 = Arch.LAL_1_2
mapLicense LAL_1_3 = Arch.LAL_1_3
mapLicense Latex2e = Arch.Latex2e
mapLicense Leptonica = Arch.Leptonica
mapLicense LGPL_2_0_only = Arch.LGPL_2_0_only
mapLicense LGPL_2_0_or_later = Arch.LGPL_2_0_or_later
mapLicense LGPL_2_1_only = Arch.LGPL_2_1_only
mapLicense LGPL_2_1_or_later = Arch.LGPL_2_1_or_later
mapLicense LGPL_3_0_only = Arch.LGPL_3_0_only
mapLicense LGPL_3_0_or_later = Arch.LGPL_3_0_or_later
mapLicense LGPLLR = Arch.LGPLLR
mapLicense Libpng_2_0 = Arch.Libpng_2_0
mapLicense Libpng = Arch.Libpng
mapLicense Libselinux_1_0 = Arch.Libselinux_1_0
mapLicense Libtiff = Arch.Libtiff
mapLicense LiLiQ_P_1_1 = Arch.LiLiQ_P_1_1
mapLicense LiLiQ_R_1_1 = Arch.LiLiQ_R_1_1
mapLicense LiLiQ_Rplus_1_1 = Arch.LiLiQ_Rplus_1_1
mapLicense Linux_man_pages_copyleft = Arch.Linux_man_pages_copyleft
mapLicense Linux_OpenIB = Arch.Linux_OpenIB
mapLicense LPL_1_02 = Arch.LPL_1_02
mapLicense LPL_1_0 = Arch.LPL_1_0
mapLicense LPPL_1_0 = Arch.LPPL_1_0
mapLicense LPPL_1_1 = Arch.LPPL_1_1
mapLicense LPPL_1_2 = Arch.LPPL_1_2
mapLicense LPPL_1_3a = Arch.LPPL_1_3a
mapLicense LPPL_1_3c = Arch.LPPL_1_3c
mapLicense MakeIndex = Arch.MakeIndex
mapLicense MirOS = Arch.MirOS
mapLicense MIT_0 = Arch.MIT_0
mapLicense MIT_advertising = Arch.MIT_advertising
mapLicense MIT_CMU = Arch.MIT_CMU
mapLicense MIT_enna = Arch.MIT_enna
mapLicense MIT_feh = Arch.MIT_feh
mapLicense MIT_Modern_Variant = Arch.MIT_Modern_Variant
mapLicense MIT_open_group = Arch.MIT_open_group
mapLicense MITNFA = Arch.MITNFA
mapLicense MIT = Arch.MIT
mapLicense Motosoto = Arch.Motosoto
mapLicense Mpich2 = Arch.Mpich2
mapLicense MPL_1_0 = Arch.MPL_1_0
mapLicense MPL_1_1 = Arch.MPL_1_1
mapLicense MPL_2_0_no_copyleft_exception = Arch.MPL_2_0_no_copyleft_exception
mapLicense MPL_2_0 = Arch.MPL_2_0
mapLicense MS_PL = Arch.MS_PL
mapLicense MS_RL = Arch.MS_RL
mapLicense MTLL = Arch.MTLL
mapLicense MulanPSL_1_0 = Arch.MulanPSL_1_0
mapLicense MulanPSL_2_0 = Arch.MulanPSL_2_0
mapLicense Multics = Arch.Multics
mapLicense Mup = Arch.Mup
mapLicense NAIST_2003 = Arch.NAIST_2003
mapLicense NASA_1_3 = Arch.NASA_1_3
mapLicense Naumen = Arch.Naumen
mapLicense NBPL_1_0 = Arch.NBPL_1_0
mapLicense NCGL_UK_2_0 = Arch.NCGL_UK_2_0
mapLicense NCSA = Arch.NCSA
mapLicense Net_SNMP = Arch.Net_SNMP
mapLicense NetCDF = Arch.NetCDF
mapLicense Newsletr = Arch.Newsletr
mapLicense NGPL = Arch.NGPL
mapLicense NIST_PD_fallback = Arch.NIST_PD_fallback
mapLicense NIST_PD = Arch.NIST_PD
mapLicense NLOD_1_0 = Arch.NLOD_1_0
mapLicense NLOD_2_0 = Arch.NLOD_2_0
mapLicense NLPL = Arch.NLPL
mapLicense Nokia = Arch.Nokia
mapLicense NOSL = Arch.NOSL
mapLicense Noweb = Arch.Noweb
mapLicense NPL_1_0 = Arch.NPL_1_0
mapLicense NPL_1_1 = Arch.NPL_1_1
mapLicense NPOSL_3_0 = Arch.NPOSL_3_0
mapLicense NRL = Arch.NRL
mapLicense NTP_0 = Arch.NTP_0
mapLicense NTP = Arch.NTP
mapLicense O_UDA_1_0 = Arch.O_UDA_1_0
mapLicense OCCT_PL = Arch.OCCT_PL
mapLicense OCLC_2_0 = Arch.OCLC_2_0
mapLicense ODbL_1_0 = Arch.ODbL_1_0
mapLicense ODC_By_1_0 = Arch.ODC_By_1_0
mapLicense OFL_1_0_no_RFN = Arch.OFL_1_0_no_RFN
mapLicense OFL_1_0_RFN = Arch.OFL_1_0_RFN
mapLicense OFL_1_0 = Arch.OFL_1_0
mapLicense OFL_1_1_no_RFN = Arch.OFL_1_1_no_RFN
mapLicense OFL_1_1_RFN = Arch.OFL_1_1_RFN
mapLicense OFL_1_1 = Arch.OFL_1_1
mapLicense OGC_1_0 = Arch.OGC_1_0
mapLicense OGDL_Taiwan_1_0 = Arch.OGDL_Taiwan_1_0
mapLicense OGL_Canada_2_0 = Arch.OGL_Canada_2_0
mapLicense OGL_UK_1_0 = Arch.OGL_UK_1_0
mapLicense OGL_UK_2_0 = Arch.OGL_UK_2_0
mapLicense OGL_UK_3_0 = Arch.OGL_UK_3_0
mapLicense OGTSL = Arch.OGTSL
mapLicense OLDAP_1_1 = Arch.OLDAP_1_1
mapLicense OLDAP_1_2 = Arch.OLDAP_1_2
mapLicense OLDAP_1_3 = Arch.OLDAP_1_3
mapLicense OLDAP_1_4 = Arch.OLDAP_1_4
mapLicense OLDAP_2_0_1 = Arch.OLDAP_2_0_1
mapLicense OLDAP_2_0 = Arch.OLDAP_2_0
mapLicense OLDAP_2_1 = Arch.OLDAP_2_1
mapLicense OLDAP_2_2_1 = Arch.OLDAP_2_2_1
mapLicense OLDAP_2_2_2 = Arch.OLDAP_2_2_2
mapLicense OLDAP_2_2 = Arch.OLDAP_2_2
mapLicense OLDAP_2_3 = Arch.OLDAP_2_3
mapLicense OLDAP_2_4 = Arch.OLDAP_2_4
mapLicense OLDAP_2_5 = Arch.OLDAP_2_5
mapLicense OLDAP_2_6 = Arch.OLDAP_2_6
mapLicense OLDAP_2_7 = Arch.OLDAP_2_7
mapLicense OLDAP_2_8 = Arch.OLDAP_2_8
mapLicense OML = Arch.OML
mapLicense OpenSSL = Arch.OpenSSL
mapLicense OPL_1_0 = Arch.OPL_1_0
mapLicense OPUBL_1_0 = Arch.OPUBL_1_0
mapLicense OSET_PL_2_1 = Arch.OSET_PL_2_1
mapLicense OSL_1_0 = Arch.OSL_1_0
mapLicense OSL_1_1 = Arch.OSL_1_1
mapLicense OSL_2_0 = Arch.OSL_2_0
mapLicense OSL_2_1 = Arch.OSL_2_1
mapLicense OSL_3_0 = Arch.OSL_3_0
mapLicense Parity_6_0_0 = Arch.Parity_6_0_0
mapLicense Parity_7_0_0 = Arch.Parity_7_0_0
mapLicense PDDL_1_0 = Arch.PDDL_1_0
mapLicense PHP_3_01 = Arch.PHP_3_01
mapLicense PHP_3_0 = Arch.PHP_3_0
mapLicense Plexus = Arch.Plexus
mapLicense PolyForm_Noncommercial_1_0_0 = Arch.PolyForm_Noncommercial_1_0_0
mapLicense PolyForm_Small_Business_1_0_0 = Arch.PolyForm_Small_Business_1_0_0
mapLicense PostgreSQL = Arch.PostgreSQL
mapLicense PSF_2_0 = Arch.PSF_2_0
mapLicense Psfrag = Arch.Psfrag
mapLicense Psutils = Arch.Psutils
mapLicense Python_2_0 = Arch.Python_2_0
mapLicense Qhull = Arch.Qhull
mapLicense QPL_1_0 = Arch.QPL_1_0
mapLicense Rdisc = Arch.Rdisc
mapLicense RHeCos_1_1 = Arch.RHeCos_1_1
mapLicense RPL_1_1 = Arch.RPL_1_1
mapLicense RPL_1_5 = Arch.RPL_1_5
mapLicense RPSL_1_0 = Arch.RPSL_1_0
mapLicense RSA_MD = Arch.RSA_MD
mapLicense RSCPL = Arch.RSCPL
mapLicense Ruby = Arch.Ruby
mapLicense SAX_PD = Arch.SAX_PD
mapLicense Saxpath = Arch.Saxpath
mapLicense SCEA = Arch.SCEA
mapLicense SchemeReport = Arch.SchemeReport
mapLicense Sendmail_8_23 = Arch.Sendmail_8_23
mapLicense Sendmail = Arch.Sendmail
mapLicense SGI_B_1_0 = Arch.SGI_B_1_0
mapLicense SGI_B_1_1 = Arch.SGI_B_1_1
mapLicense SGI_B_2_0 = Arch.SGI_B_2_0
mapLicense SHL_0_51 = Arch.SHL_0_51
mapLicense SHL_0_5 = Arch.SHL_0_5
mapLicense SimPL_2_0 = Arch.SimPL_2_0
mapLicense SISSL_1_2 = Arch.SISSL_1_2
mapLicense SISSL = Arch.SISSL
mapLicense Sleepycat = Arch.Sleepycat
mapLicense SMLNJ = Arch.SMLNJ
mapLicense SMPPL = Arch.SMPPL
mapLicense SNIA = Arch.SNIA
mapLicense Spencer_86 = Arch.Spencer_86
mapLicense Spencer_94 = Arch.Spencer_94
mapLicense Spencer_99 = Arch.Spencer_99
mapLicense SPL_1_0 = Arch.SPL_1_0
mapLicense SSH_OpenSSH = Arch.SSH_OpenSSH
mapLicense SSH_short = Arch.SSH_short
mapLicense SSPL_1_0 = Arch.SSPL_1_0
mapLicense SugarCRM_1_1_3 = Arch.SugarCRM_1_1_3
mapLicense SWL = Arch.SWL
mapLicense TAPR_OHL_1_0 = Arch.TAPR_OHL_1_0
mapLicense TCL = Arch.TCL
mapLicense TCP_wrappers = Arch.TCP_wrappers
mapLicense TMate = Arch.TMate
mapLicense TORQUE_1_1 = Arch.TORQUE_1_1
mapLicense TOSL = Arch.TOSL
mapLicense TU_Berlin_1_0 = Arch.TU_Berlin_1_0
mapLicense TU_Berlin_2_0 = Arch.TU_Berlin_2_0
mapLicense UCL_1_0 = Arch.UCL_1_0
mapLicense Unicode_DFS_2015 = Arch.Unicode_DFS_2015
mapLicense Unicode_DFS_2016 = Arch.Unicode_DFS_2016
mapLicense Unicode_TOU = Arch.Unicode_TOU
mapLicense Unlicense = Arch.Unlicense
mapLicense UPL_1_0 = Arch.UPL_1_0
mapLicense Vim = Arch.Vim
mapLicense VOSTROM = Arch.VOSTROM
mapLicense VSL_1_0 = Arch.VSL_1_0
mapLicense W3C_19980720 = Arch.W3C_19980720
mapLicense W3C_20150513 = Arch.W3C_20150513
mapLicense W3C = Arch.W3C
mapLicense Watcom_1_0 = Arch.Watcom_1_0
mapLicense Wsuipa = Arch.Wsuipa
mapLicense WTFPL = Arch.WTFPL
mapLicense X11_distribute_modifications_variant = Arch.X11_distribute_modifications_variant
mapLicense X11 = Arch.X11
mapLicense Xerox = Arch.Xerox
mapLicense XFree86_1_1 = Arch.XFree86_1_1
mapLicense Xinetd = Arch.Xinetd
mapLicense Xnet = Arch.Xnet
mapLicense Xpp = Arch.Xpp
mapLicense XSkat = Arch.XSkat
mapLicense YPL_1_0 = Arch.YPL_1_0
mapLicense YPL_1_1 = Arch.YPL_1_1
mapLicense Zed = Arch.Zed
mapLicense Zend_2_0 = Arch.Zend_2_0
mapLicense Zimbra_1_3 = Arch.Zimbra_1_3
mapLicense Zimbra_1_4 = Arch.Zimbra_1_4
mapLicense Zlib_acknowledgement = Arch.Zlib_acknowledgement
mapLicense Zlib = Arch.Zlib
mapLicense ZPL_1_1 = Arch.ZPL_1_1
mapLicense ZPL_2_0 = Arch.ZPL_2_0
mapLicense ZPL_2_1 = Arch.ZPL_2_1
mapLicense x = Arch.Custom (licenseId x)

-- | Show an archlinux license
showArchLicense :: Arch.License -> String
showArchLicense = Arch.licenseId

-- | Apply 'PkgBuild' to 'felixTemplate'.
applyTemplate :: PkgBuild -> String
applyTemplate PkgBuild {..} =
  unpack $
    felixTemplate
      (pack _hkgName)
      (pack _pkgName)
      (pack _pkgVer)
      (pack _pkgDesc)
      (pack _url)
      (pack _license)
      (pack _depends)
      (pack _makeDepends)
      (pack _sha256sums)
      ( case _licenseFile of
          Just n -> "\n" <> installLicense (pack n)
          _ -> "\n"
      )
      (if _enableUusi then "\n" <> (prepare . pack $ "uusi" <> (_removeWithUusi <&> (\r -> " -u " <> r) & mconcat)) <> "\n\n" else "\n")
      ("\n" <> check <> "\n\n")
      ( pack $ case _flags of
          [] -> ""
          xs -> "\\\n" <> xs
      )

-- | Text of @check()@ function.
check :: Text
check =
  [text|
  check() {
    cd $$_hkgname-$$pkgver
    runhaskell Setup test
  }
|]

-- | Text of statements which install license.
installLicense :: Text -> Text
installLicense licenseFile =
  [text|
    install -D -m644 $licenseFile -t "$$pkgdir"/usr/share/licenses/$$pkgname/
    rm -f "$$pkgdir"/usr/share/doc/$$pkgname/$licenseFile
|]

prepare :: Text -> Text
prepare uusi =
  [text|
  prepare() {
    cd $$_hkgname-$$pkgver
    $uusi
  }
|]

-- | A fixed template of haskell package in archlinux. See <https://wiki.archlinux.org/index.php/Haskell_package_guidelines Haskell package guidelines> .
felixTemplate :: Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text
felixTemplate hkgname pkgname pkgver pkgdesc url license depends makedepends sha256sums licenseF uusiF checkF flags =
  [text|
  # This file was generated by https://github.com/berberman/arch-hs, please check it manually.
  # Maintainer: Your Name <youremail@domain.com>

  _hkgname=$hkgname
  pkgname=$pkgname
  pkgver=$pkgver
  pkgrel=1
  pkgdesc='${pkgdesc}'
  url='${url}'
  license=("$license")
  arch=('x86_64')
  depends=('ghc-libs'$depends)
  makedepends=('ghc'$makedepends)
  source=("https://hackage.haskell.org/packages/archive/$$_hkgname/$$pkgver/$$_hkgname-$$pkgver.tar.gz")
  sha256sums=($sha256sums)
  $uusiF
  build() {
    cd $$_hkgname-$$pkgver

    runhaskell Setup configure -O --enable-shared --enable-debug-info --enable-executable-dynamic --disable-library-vanilla \
      --prefix=/usr --docdir=/usr/share/doc/$$pkgname --datasubdir=$$pkgname --enable-tests \
      --dynlibdir=/usr/lib --libsubdir=\$$compiler/site-local/\$$pkgid \
      --ghc-option=-optl-Wl\,-z\,relro\,-z\,now \
      --ghc-option='-pie' $flags

    runhaskell Setup build
    runhaskell Setup register --gen-script
    runhaskell Setup unregister --gen-script
    sed -i -r -e "s|ghc-pkg.*update[^ ]* |&'--force' |" register.sh
    sed -i -r -e "s|ghc-pkg.*unregister[^ ]* |&'--force' |" unregister.sh
  }
  $checkF
  package() {
    cd $$_hkgname-$$pkgver

    install -D -m744 register.sh "$$pkgdir"/usr/share/haskell/register/$$pkgname.sh
    install -D -m744 unregister.sh "$$pkgdir"/usr/share/haskell/unregister/$$pkgname.sh
    runhaskell Setup copy --destdir="$$pkgdir"$licenseF
  }
|]
