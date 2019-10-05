import firebase from 'firebase/app'
import 'firebase/auth'
import 'firebase/firestore'
import { identity, isEmpty } from 'ramda'

const firebaseConfig = {
  apiKey: 'AIzaSyBVS1Tx23pScQz9w4ZDTGh307mqkCRy2Bw',
  authDomain: 'not-now-142808.firebaseapp.com',
  databaseURL: 'https://not-now-142808.firebaseio.com',
  projectId: 'not-now-142808',
  storageBucket: 'not-now-142808.appspot.com',
  messagingSenderId: '476064436883',
  appId: '1:476064436883:web:864772dedd00a8a1ed7366',
}

firebase.initializeApp(firebaseConfig)

const auth = firebase.auth()
const db = firebase.firestore()

function userCollectionRef(name) {
  invariant(!isEmpty(name.trim()))
  const uid = auth.currentUser.uid
  invariant(uid instanceof String)
  return db.collection(`/users/${uid}/elm-todoist-clone-v2/db/${name}`)
}

export default {
  onAuthStateChanged: auth.onAuthStateChanged.bind(auth),
  signIn() {
    const provider = new firebase.auth.GoogleAuthProvider()
    provider.setCustomParameters({})
    return auth.signInWithPopup(provider)
  },
  signOut() {
    return auth.signOut()
  },
  setAll(name, docs) {
    invariant(docs instanceof Array)
    const cr = userCollectionRef(name)
    const b = db.batch()

    docs.forEach(d => b.set(cr.doc(d.id), d))

    return b.commit()
  },
  onUserCollection(name, cb) {
    let l2 = identity
    if (auth.currentUser) {
      l2 = userCollectionRef(name).onSnapshot(cb)
    }
    const l = auth.onAuthStateChanged(() => {
      l2()
      if (auth.currentUser) {
        l2 = userCollectionRef(name).onSnapshot(cb)
      }
    })
    return () => {
      l()
      l2()
    }
  },
}

function invariant(bool, msg = 'inv failed') {
  if (!bool) {
    throw new Error(msg)
  }
}
