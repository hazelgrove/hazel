for f in `find .. -name '*.re'` ; \
do ( \
refmt -w 80 --in-place $f; \
); \
done

