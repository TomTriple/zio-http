/*
 * Copyright 2021 - 2023 Sporta Technologies PVT LTD & the ZIO HTTP contributors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio.http.endpoint.internal

import zio._

import zio.stacktracer.TracingImplicits.disableAutoTrace // scalafix:ok;

private[http] class Memoized[K, A] private (compute: K => A) { self =>
  private var map: Map[K, A] = Map()

  def get(api: K): A = {
    map.get(api) match {
      case Some(a) => a
      case None    =>
        val a = compute(api)
        map = map.updated(api, a)
        a
    }
  }
}
private[http] object Memoized                                {
  def apply[K, A](compute: K => A): Memoized[K, A] = new Memoized(compute)
}

private[http] class MemoizedZIO[K, E, A] private (compute: K => IO[E, A]) { self =>
  private val mapRef: Ref[Map[K, Promise[E, A]]] = Ref.unsafe.make(Map[K, Promise[E, A]]())(Unsafe.unsafe)

  def get(k: K)(implicit trace: Trace): IO[E, A] = {
    ZIO.fiberIdWith { fiberId =>
      for {
        p      <- Promise.make[E, A]
        effect <- mapRef.modify[IO[E, A]] { map =>
          map.get(k) match {
            case Some(promise) => (promise.await, map)
            case None          =>
              val promise = Promise.unsafe.make[E, A](fiberId)(Unsafe.unsafe)
              (compute(k).exit.tap(exit => promise.done(exit)).flatten, map + (k -> promise))
          }
        }
        result <- effect
      } yield result
    }
  }
}
private[http] object MemoizedZIO                                          {
  def apply[K, E, A](compute: K => IO[E, A]): MemoizedZIO[K, E, A] = new MemoizedZIO(compute)
}
