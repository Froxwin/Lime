OUT = ../build

NVCC = nvcc
GCC  = gcc
CFLAGS = -fPIC -O2

all: $(OUT) $(OUT)/kernel.ptx $(OUT)/driver.so

$(OUT):
	mkdir -p $(OUT)

$(OUT)/kernel.ptx: kernel.cu | $(OUT)
	$(NVCC) --ptx $< -o $@

$(OUT)/driver.o: driver.c | $(OUT)
	$(GCC) $(CFLAGS) -c $< -o $@

$(OUT)/driver.so: $(OUT)/driver.o
	$(GCC) -shared $< -lcuda -o $@

clean:
	rm -rf $(OUT)
