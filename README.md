# Wolfram

Elementary cellular automaton, also known as the Wolfram cellular automaton.

`$ ./wolfram [--rule r] [--start s] [--window w] [--lines h] [--move m]`

## Example

```
$ ./wolfram --rule 90 --lines 30 | cat -e
                                        *                                       $
                                       * *                                      $
                                      *   *                                     $
                                     * * * *                                    $
                                    *       *                                   $
                                   * *     * *                                  $
                                  *   *   *   *                                 $
                                 * * * * * * * *                                $
                                *               *                               $
                               * *             * *                              $
                              *   *           *   *                             $
                             * * * *         * * * *                            $
                            *       *       *       *                           $
                           * *     * *     * *     * *                          $
                          *   *   *   *   *   *   *   *                         $
                         * * * * * * * * * * * * * * * *                        $
                        *                               *                       $
                       * *                             * *                      $
                      *   *                           *   *                     $
                     * * * *                         * * * *                    $
                    *       *                       *       *                   $
                   * *     * *                     * *     * *                  $
                  *   *   *   *                   *   *   *   *                 $
                 * * * * * * * *                 * * * * * * * *                $
                *               *               *               *               $
               * *             * *             * *             * *              $
              *   *           *   *           *   *           *   *             $
             * * * *         * * * *         * * * *         * * * *            $
            *       *       *       *       *       *       *       *           $
           * *     * *     * *     * *     * *     * *     * *     * *          $
```

## Supported rules

All rules should be supported. But only the following rules were tested and
confirmed to work:
- Rule 30
- Rule 90
- Rule 110

# Score

Average score: 92%

| Test            | Score             |
| --------------- | ----------------- |
| Basic           | 100%              |
| Rule 30         | 100%              |
| Rule 90         | 100%              |
| Rule 110        | 100%              |
| Infinite        | 100%              |
| Window and move | 100%              |
| Performance     | 33.3%             |
| Error handling  | 100%              |
| Coding Style    | 100%              |
