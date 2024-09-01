"use strict";

import axios from "axios";
import { config } from "dotenv";

config();
let cookie = process.env.COOKIE;

export function getQuestionData(url) {
  return axios.get(url, {
    headers: {
      Accept: "application/json",
      Cookie: cookie,
    },
  });
}
