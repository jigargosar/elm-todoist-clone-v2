import firebase from 'firebase/app'

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

export default {
  onAuthStateChanged: auth.onAuthStateChanged.bind(auth),
}
