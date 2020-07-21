let id = ref(100);

let getId = () => {
    let i = id^;
    id := i + 1;
    i
}
