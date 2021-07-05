Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'LiteralValue
                                ( String "A" )
                            , alias = Nothing
                            }
                        ) :| []
                    , from = Just
                        ( Table'Subquery
                            ( Aliased
                                { value = SelectStatement
                                    { commonTableExpressions = Nothing
                                    , select = CompoundSelect
                                        ( SelectCore'Select
                                            ( Select
                                                { distinct = False
                                                , columns = ResultColumn'Expression
                                                    ( Aliased
                                                        { value = Expression'LiteralValue
                                                            ( String "B" )
                                                        , alias = Nothing
                                                        }
                                                    ) :| []
                                                , from = Nothing
                                                , where_ = Nothing
                                                , groupBy = Nothing
                                                , window = Nothing
                                                }
                                            )
                                        )
                                    , orderBy = Nothing
                                    , limit = Nothing
                                    }
                                , alias = Nothing
                                }
                            )
                        )
                    , where_ = Nothing
                    , groupBy = Nothing
                    , window = Nothing
                    }
                )
            )
        , orderBy = Just
            ( OrderingTerm
                { expression = Expression'Exists
                    ( SelectStatement
                        { commonTableExpressions = Nothing
                        , select = CompoundSelect
                            ( SelectCore'Select
                                ( Select
                                    { distinct = False
                                    , columns = ResultColumn'Expression
                                        ( Aliased
                                            { value = Expression'LiteralValue
                                                ( String "C" )
                                            , alias = Nothing
                                            }
                                        ) :| []
                                    , from = Just
                                        ( Table'Subquery
                                            ( Aliased
                                                { value = SelectStatement
                                                    { commonTableExpressions = Nothing
                                                    , select = CompoundSelect
                                                        ( SelectCore'Select
                                                            ( Select
                                                                { distinct = False
                                                                , columns = ResultColumn'Expression
                                                                    ( Aliased
                                                                        { value = Expression'LiteralValue
                                                                            ( String "D" )
                                                                        , alias = Nothing
                                                                        }
                                                                    ) :| []
                                                                , from = Nothing
                                                                , where_ = Nothing
                                                                , groupBy = Nothing
                                                                , window = Nothing
                                                                }
                                                            )
                                                        )
                                                    , orderBy = Nothing
                                                    , limit = Just
                                                        ( Limit
                                                            { limit = Expression'LiteralValue
                                                                ( Number "0" )
                                                            , offset = Nothing
                                                            }
                                                        )
                                                    }
                                                , alias = Nothing
                                                }
                                            )
                                        )
                                    , where_ = Nothing
                                    , groupBy = Nothing
                                    , window = Nothing
                                    }
                                )
                            )
                        , orderBy = Nothing
                        , limit = Nothing
                        }
                    )
                , collation = Nothing
                , ordering = Ascending
                , nullsPlacement = NullsFirst
                } :| []
            )
        , limit = Nothing
        }
    )