Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Negate
                                ( Expression'Case
                                    ( CaseExpression
                                        { base = Nothing
                                        , cases =
                                            ( Expression'And
                                                ( Expression'Not
                                                    ( Expression'Exists
                                                        ( SelectStatement
                                                            { commonTableExpressions = Nothing
                                                            , select = CompoundSelect
                                                                ( SelectCore'Select
                                                                    ( Select
                                                                        { distinct = False
                                                                        , columns = ResultColumn'Expression
                                                                            ( Aliased
                                                                                { value = Expression'LiteralValue
                                                                                    ( Number "1" )
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
                                                                            ( Expression'And
                                                                                ( Expression'Not
                                                                                    ( Expression'InValues
                                                                                        ( InValuesExpression
                                                                                            { expression = Expression'Minus
                                                                                                ( Expression'Plus
                                                                                                    ( Expression'Minus
                                                                                                        ( Expression'Multiply
                                                                                                            ( Expression'Multiply
                                                                                                                ( Expression'Column
                                                                                                                    ( Namespaced
                                                                                                                        { namespace = Nothing
                                                                                                                        , value = "e"
                                                                                                                        }
                                                                                                                    )
                                                                                                                )
                                                                                                                ( Expression'Column
                                                                                                                    ( Namespaced
                                                                                                                        { namespace = Nothing
                                                                                                                        , value = "d"
                                                                                                                        }
                                                                                                                    )
                                                                                                                )
                                                                                                            )
                                                                                                            ( Expression'Case
                                                                                                                ( CaseExpression
                                                                                                                    { base = Just
                                                                                                                        ( Expression'LiteralValue
                                                                                                                            ( Number "17" )
                                                                                                                        )
                                                                                                                    , cases =
                                                                                                                        ( Expression'Case
                                                                                                                            ( CaseExpression
                                                                                                                                { base = Just
                                                                                                                                    ( Expression'Minus
                                                                                                                                        ( Expression'Column
                                                                                                                                            ( Namespaced
                                                                                                                                                { namespace = Just
                                                                                                                                                    ( Namespaced
                                                                                                                                                        { namespace = Nothing
                                                                                                                                                        , value = "t1"
                                                                                                                                                        }
                                                                                                                                                    )
                                                                                                                                                , value = "c"
                                                                                                                                                }
                                                                                                                                            )
                                                                                                                                        )
                                                                                                                                        ( Expression'Column
                                                                                                                                            ( Namespaced
                                                                                                                                                { namespace = Just
                                                                                                                                                    ( Namespaced
                                                                                                                                                        { namespace = Nothing
                                                                                                                                                        , value = "t1"
                                                                                                                                                        }
                                                                                                                                                    )
                                                                                                                                                , value = "f"
                                                                                                                                                }
                                                                                                                                            )
                                                                                                                                        )
                                                                                                                                    )
                                                                                                                                , cases =
                                                                                                                                    ( Expression'Column
                                                                                                                                        ( Namespaced
                                                                                                                                            { namespace = Just
                                                                                                                                                ( Namespaced
                                                                                                                                                    { namespace = Nothing
                                                                                                                                                    , value = "t1"
                                                                                                                                                    }
                                                                                                                                                )
                                                                                                                                            , value = "a"
                                                                                                                                            }
                                                                                                                                        )
                                                                                                                                    , Expression'Column
                                                                                                                                        ( Namespaced
                                                                                                                                            { namespace = Just
                                                                                                                                                ( Namespaced
                                                                                                                                                    { namespace = Nothing
                                                                                                                                                    , value = "t1"
                                                                                                                                                    }
                                                                                                                                                )
                                                                                                                                            , value = "a"
                                                                                                                                            }
                                                                                                                                        )
                                                                                                                                    ) :| []
                                                                                                                                , else_ = Expression'LiteralValue
                                                                                                                                    ( Number "11" )
                                                                                                                                }
                                                                                                                            )
                                                                                                                        , Expression'Column
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Just
                                                                                                                                    ( Namespaced
                                                                                                                                        { namespace = Nothing
                                                                                                                                        , value = "t1"
                                                                                                                                        }
                                                                                                                                    )
                                                                                                                                , value = "c"
                                                                                                                                }
                                                                                                                            )
                                                                                                                        ) :| []
                                                                                                                    , else_ = Expression'Column
                                                                                                                        ( Namespaced
                                                                                                                            { namespace = Just
                                                                                                                                ( Namespaced
                                                                                                                                    { namespace = Nothing
                                                                                                                                    , value = "t1"
                                                                                                                                    }
                                                                                                                                )
                                                                                                                            , value = "b"
                                                                                                                            }
                                                                                                                        )
                                                                                                                    }
                                                                                                                )
                                                                                                            )
                                                                                                        )
                                                                                                        ( Expression'Column
                                                                                                            ( Namespaced
                                                                                                                { namespace = Nothing
                                                                                                                , value = "e"
                                                                                                                }
                                                                                                            )
                                                                                                        )
                                                                                                    )
                                                                                                    ( Expression'Column
                                                                                                        ( Namespaced
                                                                                                            { namespace = Just
                                                                                                                ( Namespaced
                                                                                                                    { namespace = Nothing
                                                                                                                    , value = "t1"
                                                                                                                    }
                                                                                                                )
                                                                                                            , value = "a"
                                                                                                            }
                                                                                                        )
                                                                                                    )
                                                                                                )
                                                                                                ( Expression'Column
                                                                                                    ( Namespaced
                                                                                                        { namespace = Nothing
                                                                                                        , value = "e"
                                                                                                        }
                                                                                                    )
                                                                                                )
                                                                                            , values =
                                                                                                [ Expression'Column
                                                                                                    ( Namespaced
                                                                                                        { namespace = Nothing
                                                                                                        , value = "e"
                                                                                                        }
                                                                                                    )
                                                                                                , Expression'Column
                                                                                                    ( Namespaced
                                                                                                        { namespace = Nothing
                                                                                                        , value = "e"
                                                                                                        }
                                                                                                    )
                                                                                                , Expression'Column
                                                                                                    ( Namespaced
                                                                                                        { namespace = Just
                                                                                                            ( Namespaced
                                                                                                                { namespace = Nothing
                                                                                                                , value = "t1"
                                                                                                                }
                                                                                                            )
                                                                                                        , value = "a"
                                                                                                        }
                                                                                                    )
                                                                                                ]
                                                                                            }
                                                                                        )
                                                                                    )
                                                                                )
                                                                                ( Expression'Not
                                                                                    ( Expression'Between
                                                                                        ( Expression'Column
                                                                                            ( Namespaced
                                                                                                { namespace = Just
                                                                                                    ( Namespaced
                                                                                                        { namespace = Nothing
                                                                                                        , value = "t1"
                                                                                                        }
                                                                                                    )
                                                                                                , value = "f"
                                                                                                }
                                                                                            )
                                                                                        )
                                                                                        ( Expression'LiteralValue
                                                                                            ( Number "19" )
                                                                                        )
                                                                                        ( Expression'Column
                                                                                            ( Namespaced
                                                                                                { namespace = Just
                                                                                                    ( Namespaced
                                                                                                        { namespace = Nothing
                                                                                                        , value = "t1"
                                                                                                        }
                                                                                                    )
                                                                                                , value = "e"
                                                                                                }
                                                                                            )
                                                                                        )
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
                                                )
                                                ( Expression'LessThan
                                                    ( Expression'Column
                                                        ( Namespaced
                                                            { namespace = Just
                                                                ( Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "t1"
                                                                    }
                                                                )
                                                            , value = "b"
                                                            }
                                                        )
                                                    )
                                                    ( Expression'Column
                                                        ( Namespaced
                                                            { namespace = Just
                                                                ( Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "t1"
                                                                    }
                                                                )
                                                            , value = "a"
                                                            }
                                                        )
                                                    )
                                                )
                                            , Expression'Plus
                                                ( Expression'Minus
                                                    ( Expression'Minus
                                                        ( Expression'Column
                                                            ( Namespaced
                                                                { namespace = Nothing
                                                                , value = "f"
                                                                }
                                                            )
                                                        )
                                                        ( Expression'Column
                                                            ( Namespaced
                                                                { namespace = Nothing
                                                                , value = "d"
                                                                }
                                                            )
                                                        )
                                                    )
                                                    ( Expression'Column
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "c"
                                                            }
                                                        )
                                                    )
                                                )
                                                ( Expression'Column
                                                    ( Namespaced
                                                        { namespace = Nothing
                                                        , value = "b"
                                                        }
                                                    )
                                                )
                                            ) :|
                                            [
                                                ( Expression'Equals
                                                    ( Expression'LiteralValue
                                                        ( Number "17" )
                                                    )
                                                    ( Expression'LiteralValue
                                                        ( Number "17" )
                                                    )
                                                , Expression'LiteralValue
                                                    ( Number "19" )
                                                )
                                            ]
                                        , else_ = Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "d"
                                                }
                                            )
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
                                        , value = "t1"
                                        }
                                    , alias = Nothing
                                    }
                                , indexedBy = Nothing
                                }
                            )
                        )
                    , where_ = Just
                        ( Expression'Not
                            ( Expression'Between
                                ( Expression'Multiply
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "f"
                                            }
                                        )
                                    )
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Just
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "t1"
                                                    }
                                                )
                                            , value = "a"
                                            }
                                        )
                                    )
                                )
                                ( Expression'FunctionCall
                                    ( FunctionCallExpression
                                        { call = FunctionCall
                                            { name = Namespaced
                                                { namespace = Nothing
                                                , value = "coalesce"
                                                }
                                            , arguments = FunctionArguments'Arguments
                                                [ Expression'Subquery
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
                                                                                    { namespace = Just
                                                                                        ( Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "t1"
                                                                                            }
                                                                                        )
                                                                                    , value = "b"
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
                                                                        ( Expression'Equals
                                                                            ( Expression'LiteralValue
                                                                                ( Number "17" )
                                                                            )
                                                                            ( Expression'LiteralValue
                                                                                ( Number "13" )
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
                                                , Expression'Multiply
                                                    ( Expression'LiteralValue
                                                        ( Number "17" )
                                                    )
                                                    ( Expression'Column
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "e"
                                                            }
                                                        )
                                                    )
                                                ]
                                            }
                                        , filter = Nothing
                                        , over = Nothing
                                        }
                                    )
                                )
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "e"
                                        }
                                    )
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