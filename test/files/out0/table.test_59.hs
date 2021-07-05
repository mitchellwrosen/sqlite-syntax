Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "t8"
            }
        , definition = Left
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
                                            , value = "b"
                                            }
                                        )
                                    , alias = Nothing
                                    }
                                ) :|
                                [ ResultColumn'Expression
                                    ( Aliased
                                        { value = Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "h"
                                                }
                                            )
                                        , alias = Nothing
                                        }
                                    )
                                , ResultColumn'Expression
                                    ( Aliased
                                        { value = Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "a"
                                                }
                                            )
                                        , alias = Just "i"
                                        }
                                    )
                                , ResultColumn'Expression
                                    ( Aliased
                                        { value = Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "f"
                                                }
                                            )
                                        , alias = Just "j"
                                        }
                                    )
                                ]
                            , from = Just
                                ( Table
                                    ( QualifiedTableName
                                        { name = Aliased
                                            { value = Namespaced
                                                { namespace = Nothing
                                                , value = "t7"
                                                }
                                            , alias = Nothing
                                            }
                                        , indexedBy = Nothing
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
        }
    )