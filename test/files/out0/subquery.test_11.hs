Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Column
                                ( Namespaced
                                    { namespace = Nothing
                                    , value = "a"
                                    }
                                )
                            , alias = Nothing
                            }
                        ) :| []
                    , from = Just
                        ( Table
                            ( QualifiedTableName
                                { name = Aliased
                                    { value = Namespaced
                                        { namespace = Nothing
                                        , value = "t1"
                                        }
                                    , alias = Nothing
                                    }
                                , indexedBy = Nothing
                                }
                            )
                        )
                    , where_ = Just
                        ( Expression'Subquery
                            ( SelectStatement
                                { commonTableExpressions = Nothing
                                , select = CompoundSelect
                                    ( SelectCore'Select
                                        ( Select
                                            { distinct = False
                                            , columns = ResultColumn'Expression
                                                ( Aliased
                                                    { value = Expression'GreaterThan
                                                        ( Expression'Multiply
                                                            ( Expression'Column
                                                                ( Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "y"
                                                                    }
                                                                )
                                                            )
                                                            ( Expression'LiteralValue
                                                                ( Number "2" )
                                                            )
                                                        )
                                                        ( Expression'Column
                                                            ( Namespaced
                                                                { namespace = Nothing
                                                                , value = "b"
                                                                }
                                                            )
                                                        )
                                                    , alias = Nothing
                                                    }
                                                ) :| []
                                            , from = Just
                                                ( Table
                                                    ( QualifiedTableName
                                                        { name = Aliased
                                                            { value = Namespaced
                                                                { namespace = Nothing
                                                                , value = "t2"
                                                                }
                                                            , alias = Nothing
                                                            }
                                                        , indexedBy = Nothing
                                                        }
                                                    )
                                                )
                                            , where_ = Just
                                                ( Expression'Equals
                                                    ( Expression'Column
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "a"
                                                            }
                                                        )
                                                    )
                                                    ( Expression'Column
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "x"
                                                            }
                                                        )
                                                    )
                                                )
                                            , groupBy = Nothing
                                            , window = Nothing
                                            }
                                        )
                                    )
                                , orderBy = Nothing
                                , limit = Nothing
                                }
                            )
                        )
                    , groupBy = Nothing
                    , window = Nothing
                    }
                )
            )
        , orderBy = Nothing
        , limit = Nothing
        }
    )