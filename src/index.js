import './index.css'
import { Elm } from './Main.elm'

Elm.Main.init({
  flags: { cache: { todoList: [], epoch: Date.now() } },
  node: document.getElementById('root'),
})
