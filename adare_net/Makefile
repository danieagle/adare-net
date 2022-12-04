
.PHONY: all all-debug clean install install-dev install-debug install-dev-debug uninstall os-verify

all: clean
	gprbuild -j8 -Xmod=release -p -Xmi_os=${AOS} -Padare_net

all-debug: clean
	gprbuild -j8 -Xmod=debug -p -Xmi_os=${AOS} -Padare_net

clean: os-verify
	gprclean -Xmi_os=${AOS} -Padare_net

install: os-verify
	gprinstall --mode=usage -Xmi_os=${AOS} -p -Padare_net

install-dev: os-verify
	gprinstall --mode=dev -Xmi_os=${AOS} -p -Padare_net

uninstall:
	gprinstall --uninstall -Padare_net

os-verify:
ifndef AOS
	@echo "I need the AOS var to compile, to clean and to install ";
	@echo
	@echo "make AOS=OS_TYPE ";
	@echo
	@echo "OS_TYPE is one of linux, mswindows, macosx, bsd, unix, solaris, other";
	@echo
	@echo "note: If you need other OS's just file a issue in";
	@echo "https://github.com/danieagle/adare-net;
	@echo
	@exit 1;
endif
