import { useState } from 'react'
import styles from './index.module.css'

export default function Counter() {
  const [count, setCount] = useState(0)

  return (
    <div className={styles.container}>
      <h1>Counter: {count}</h1>
      <button onClick={() => setCount(count + 1)}>Increment</button>
      <button onClick={() => setCount(count - 1)}>Decrement</button>
      <button onClick={() => setCount(0)}>Reset</button>
    </div>
  )
}
