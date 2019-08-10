#!/bin/bash

# Set the static increment value.  Keep in mind that this will
# be done twice.

# Get the Maximum value for use.
#MaxVal=$(cat /sys/class/backlight/intel_backlight/max_brightness);
read -r MaxVal < "/sys/class/backlight/intel_backlight/max_brightness"

IncVal=$((MaxVal/100))

# Get the current brightness value.
#CurrVal=$(cat /sys/class/backlight/intel_backlight/brightness);
read -r CurrVal < "/sys/class/backlight/intel_backlight/brightness"

# Set the new value minus the decrement value.
NewVal=$(($CurrVal + $IncVal));
echo $NewVal


# Set it to the threshold of the max value.
ThresholdVal=$(($NewVal<$MaxVal?$NewVal:$MaxVal))
echo $ThresholdVal

# Set the new value directly.
echo -n $ThresholdVal > /sys/class/backlight/intel_backlight/brightness

logger "[ACPI] brightnessup |$CurrVal| |$NewVal| |$ThresholdVal|"
