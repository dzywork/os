@echo off
echo 删除build
rm -rf build 
echo 执行make_boot_bin.bat
call ./make_boot_bin.bat
echo 执行./make_boot_img.bat
call ./make_boot_img.bat