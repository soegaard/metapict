beginfig(1)
  path p;
  p := (1,2){curl 1} ..(3,4) ..(1,2) ..{curl 1}(1,2);
  show p;
  draw p;
endfig;
