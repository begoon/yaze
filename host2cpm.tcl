#!/usr/bin/tcl

# copies host directories to/from a cpm disk image file
# Note a single file can be
# moved with the yaze cdm command.

# Coded by Gary L. Howell, Jan. 2001

# uses package command line from the standard tcllib
package require cmdline 1.0
namespace import cmdline::getopt

set usage "
host2cpm \[-r\] \[-d 'dirpath\[filter\]'\] \[-cpm 'filepath'\] \[-f 'filter'\]
host2cpm -h
   Moves multiple files in a dir to/from the cpm disk.
      -r optional - reverse cpm->host, instead of default host->cpm
      -d optional - the directory pathname  of the host directory
         to be copied/updated.
         For host->cpm, if the path is a directory name, all the files in the
         directory will be copied. If the path includes a quoted glob pattern,
         only the selected files will be copied. Example '-d 'work/cpm/*.ASM'.
         Glob patterns must be quoted so that the shell does not expand them
         before passing the argument to the script.
         For (-r) reverse mode cpm->host just the directory path should be
         supplied, all files  from the cpm disk will be copied to the host
         directory unless the a filter is supplied ( see the -f option ).
         Defaults to the current dir.
      -cpm optional - the file of the cpm disk image. Default is the name of the 
         current dir with .dsk appended. For direction host->cpm if the file does
         not exist, a new empty disk file is created.
      -f optional - the CPM file glob pattern selects files to be copied.
         EX -f '*.ASM' - Must be quoted from the shell. 
      -h help - only display this message. "

variable reverse 0
variable hostpath ""
variable cpmfile ""
variable cpmfilter ""

proc process_command_line_options {} {
    set arguments_definition { h r d.arg cpm.arg f.arg }
    set  option ""
    set  option_value ""
    global argc argv usage
    variable reverse 
    variable hostpath
    variable cpmfile
    variable cpmfilter

    while { 
	[set result [ cmdline::getopt argv $arguments_definition option option_value ]] != 0
    } { 
	if { $result == -1 } then { puts "Command line $option_value. Try host2cpm -h for help." ; exit 1 }
	switch $option {
	    r { set reverse 1 }
	    d { set hostpath $option_value }
	    cpm { set cpmfile $option_value }
	    f { set cpmfilter $option_value }
	    h { puts $usage ; exit 0 }
	}
    }
}

process_command_line_options

if { $cpmfile == "" } then { set cpmfile "[file tail [ pwd ]].dsk" }

if {$reverse} then {
    set fcdm [ open "|cdm $cpmfile" r+ ]

    puts $fcdm "dir $cpmfilter"
    puts $fcdm quit
    flush $fcdm

    set cpm_file_directory [ read $fcdm ]
    close $fcdm

    set cpm_files {}
    foreach word $cpm_file_directory {
	if {[ string match 0:* $word ]} {
	    lappend cpm_files $word
	}
    }

    set fcdm [ open "|cdm $cpmfile" w ]
    
    foreach cpmfile $cpm_files {
	puts $fcdm "cp $cpmfile u:$hostpath"
    }
    puts $fcdm quit
    close $fcdm
    
} else {
    if {$hostpath == ""} then {
	set hostpath *
    } elseif { [ string match */ $hostpath ]  } then {
	set hostpath "$hostpath*"
    } elseif { ! [string match {*/*\**} $hostpath] } then {
	set hostpath "$hostpath/*"
    } 

    set fcdm [ open "|cdm " w ]

    if { ! [ file exists $cpmfile ] } then {
	puts "host2cpm: Creating a new cpm disk file $cpmfile."
	puts $fcdm "create $cpmfile"
    }
    puts $fcdm "mount a $cpmfile"

    foreach unixfile [ glob "$hostpath" ] {
	if { ( [file type $unixfile ] == "file" ) && ( $unixfile != $cpmfile )} then {
	puts $fcdm "cp u:$unixfile a:[file tail $unixfile ]"
	}
    }
    puts $fcdm quit
    close $fcdm
}

