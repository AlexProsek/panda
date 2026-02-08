// ===========================================
// test_mat_gen.sci
//
// CLI Usage:
// wscilex-cli.exe -nb -f test_mat_gen.sci
//
// Purpose:
// Generate MAT v4 test files for Delphi importer
// ===========================================

scriptdir = fileparts(get_absolute_file_path("test_mat_gen.sci"));

// numeric matrices

vui8 = uint8([1 2 3]);
vi16 = int16([-2000 -1000 0 1000 2000]);
vui16 = uint16([0 1000 2000]);
vi32 = int32([-2000000 -1000000 0 1000000 2000000]);
vd = [1 2 3];

mui8 = uint8([1 2 3; 4 5 6]);

fn = fullfile(scriptdir, "numMat.mat");
savematfile(fn, "-v4", ["vui8" "vi16" "vui16" "vi32" "vd" "mui8"]);


// complex matrices

v = [1+2j 3+4j];
m = [1+2j 3+4j 5+6j; 7+8j 9+10j 11+12j];

fn = fullfile(scriptdir, "cmplxMat.mat");
savematfile(fn, "-v4", ["v" "m"]);

// string vector

v = ["bubu" "haha" "c"];

fn = fullfile(scriptdir, "strMat.mat");
savematfile(fn, "-v4", ["v"]);

mode = getscilabmode();
if mode == "NW" || mode == "NWNI" then
  exit;  
end
