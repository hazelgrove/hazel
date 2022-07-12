self.onmessage = (e) => {
  console.log(e.data.c);
  const num = Math.floor(Math.random * 100);
  postMessage(`((IntLit ${num})()(BoxedValue(IntLit ${num})))`);
};
