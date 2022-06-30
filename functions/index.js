const functions = require('firebase-functions');
const express = require('express');
const cors = require("cors");
const stripe = require('stripe')('sk_test_m53VFzapqyTIyodYFdd1H20y00CRvmN0zc');
const app = express();
app.use(cors({ origin: true }));
app.use(cors({
  origin: true
}));
app.use(express.json());
var corsOptions = {
  origin: 'http://localhost:3000',
  optionsSuccessStatus: 200 // some legacy browsers (IE11, various SmartTVs) choke on 204
}
app.post('/payments/create', cors(corsOptions), async (req, res) => {

  try {
    const { amount, shipping } = req.body;
    const paymentIntent = await stripe.paymentIntents.create({
      shipping,
      amount,
      currency: 'usd'
    });

    res
      .status(200)
      .send(paymentIntent.client_secret);

  } catch (err) {
    console.log(err.message);
    res
      .status(500)
      .json({
        statusCode: 500,
        message: err.message
      });
  }
})

app.get('*', (req, res) => {
  res
    .status(404)
    .send('404, Not Found.');
});

exports.api = functions.https.onRequest(app);
