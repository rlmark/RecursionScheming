package int_list

case class Fix[F[_]](unfix: F[Fix[F]]) {

}
object Fix {
  // Todo: decide which one you like
  def out[F[_]](fix: Fix[F]): F[Fix[F]] = fix.unfix
//  def out[F[_]](): Fix[F] => F[Fix[F]] = { fix: Fix[F] => fix.unfix}

  def in[F[_]](f: F[Fix[F]]): Fix[F] = Fix(f)
}