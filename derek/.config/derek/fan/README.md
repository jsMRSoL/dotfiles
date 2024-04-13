# To stop the fan going full speed all the time
paru -Syu i8kutils dell-bios-fan-control.git
cp ./modprobe.d/dell-smm-hwmon.conf /etc/modprobe.d/
systemctl enable --now dell-bios-fan-control i8kmon
