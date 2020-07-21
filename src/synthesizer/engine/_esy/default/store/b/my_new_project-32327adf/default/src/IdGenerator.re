let id = ref(0);

let getId = () => {
    let i = id^;
    id := i + 1;
    i
}
