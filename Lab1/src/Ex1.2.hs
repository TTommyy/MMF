-- Tomasz Koczar --
-- 2023AD --

-- Modele Matematyki Finansowej --
-- Zestaw I --

{-
  I.3. Zakładamy roczną wartość inflacji 12%. Dostępna jest roczna lokata oprocentowana kwartalnie stopą nominalną
  8%. Znaleźć dokładną realną stopę zwrotu z inwestycji w tę lokatę oraz korzystając z przybliżonego wzoru Fishera
  (czyli różnica inflacji oraz stóp procentowych).
  Powtórzyć wyliczenia z uwzględnieniem podatku od zysków kapitałowych w wysokości 19% od naliczonych odsetek,
  przy czym rozważyć przypadki
  – podatek jest pobierany przy każdej kapitalizacji odsetek;
  – podatek jest pobierany na koniec inwestycji, czyli po roku
-}

{-
  p - nominalne oprocentowanie
  r - efektywne oprecentowanie
  i - inflacja
  m - liczba kapitalizaji w roku
  x - rzeczywisty zysk

  rzeczywisata stopa zwortu:
  x = (1 + r)(1/(1 + i)) - 1
  przyblizone_rownianie_fishera:
  r = n - i

  roznica w obliczinach bedzie zawierala sie w kwesti
  obliczania r z podatkiem lub bez

-}

tax_ratio :: Float
tax_ratio = 0.81

-- | Oblicza rzeczywiste oprecentowanie
-- @param p - nominalne oprecentowanie
-- @param m - liczba kapitalizacji w roku
-- @returns w liczbach nie procentach
calculate_effective_intrest_rates_discrete_time_no_tax :: Float -> Float -> Float
calculate_effective_intrest_rates_discrete_time_no_tax p m = (1 + p/m) **m - 1

calculate_effective_intrest_rates_discrete_time_final_tax :: Float -> Float -> Float
calculate_effective_intrest_rates_discrete_time_final_tax p m = ((1 + p/m) **m - 1) * tax_ratio

calculate_effective_intrest_rates_discrete_time_perioid_tax :: Float -> Float -> Float
calculate_effective_intrest_rates_discrete_time_perioid_tax p m = ((1 + (p/m) * tax_ratio) **m - 1)

-- | Oblicza dokladna stope zwrotu
-- @param r - efektywne oprecentowanie
-- @praam i - inflacja
-- @returns x * 100
calculate_effectife_rate_of_return :: Float -> Float -> Float
calculate_effectife_rate_of_return r i = (1 + r) * (1/(1 + i)) * 100 - 100

-- | Oblicza przyblizana stope zwrotu
-- @param r - efektywne oprecentowanie
-- @praam i - inflacja
-- @returns x * 100
calculate_approx_rate_of_return :: Float -> Float -> Float
calculate_approx_rate_of_return r i = (r - i) * 100

main :: IO()
main = do
-- Dane zadania --
  let p = 0.08
  let m = 4
  let i = 0.12

-- rrso --
  let r_no_tax = calculate_effective_intrest_rates_discrete_time_no_tax p m
  let r_final_tax = calculate_effective_intrest_rates_discrete_time_final_tax p m
  let r_period_tax = calculate_effective_intrest_rates_discrete_time_perioid_tax p m

  let rrs = [r_no_tax, r_final_tax, r_period_tax]

-- sprytne lambddy --
  let calculate_effective_with_fixed_i = (\r -> calculate_effectife_rate_of_return r i)
  let calculate_approx_with_fixed_i = (\r -> calculate_approx_rate_of_return r i)

-- oblicznia --
  let effective = map calculate_effective_with_fixed_i rrs
  let approx = map calculate_approx_with_fixed_i rrs

-- output --
  putStrLn "Effective rates:"
  print (map (\x -> (x * 100)) rrs)

  putStrLn "Effective ror:"
  print effective

  putStrLn "Approx ror:"
  print approx

{-
  Effective rates:
  [8.243203,6.6769943,6.639147]
  Effective ror:
  [-3.3542786,-4.7526855,-4.786476]
  Approx ror:
  [-3.7567966,-5.3230057,-5.3608527]
-}