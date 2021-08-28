echo Y | rmdir /s release
sbcl --load build-develop.lisp
call build-frontend.bat
mkdir release
mkdir release\lde
echo F | xcopy lde.exe release\lde\lde.exe
echo D | xcopy /s /h /e /r /k /y document-root release\lde\document-root
echo D | xcopy /s /h /e /r /k /y templates release\lde\templates
timeout 10



