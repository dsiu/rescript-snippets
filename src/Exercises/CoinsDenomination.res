let log = Console.log

let dem = [1, 3, 5]
let amount = 100

let sortArrayDesc = Array.toSorted(_, (a,b)=>Int.compare(a,b)->Ordering.invert)

let find = (dem, amount) => {

    // result ({5:2, 3:, 1:2} , amountLeft)
    dem->sortArrayDesc->Array.reduce((Map.make(), amount), ((collected, left), x) => {
      let n = left / x
      let remaining = left % x
      collected->Map.set(x, n)
        (collected, remaining)
    })
}

find(dem, 1)->log
find(dem, 2)->log
find(dem, 3)->log
find(dem, 10)->log
find(dem, 11)->log
find(dem, 12)->log
find(dem, 13)->log


