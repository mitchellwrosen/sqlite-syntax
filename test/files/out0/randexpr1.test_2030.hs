Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Multiply
                                ( Expression'Case
                                    ( CaseExpression
                                        { base = Just
                                            ( Expression'LiteralValue
                                                ( Number "13" )
                                            )
                                        , cases =
                                            ( Expression'LiteralValue
                                                ( Number "13" )
                                            , Expression'Case
                                                ( CaseExpression
                                                    { base = Nothing
                                                    , cases =
                                                        ( Expression'Between
                                                            ( Expression'LiteralValue
                                                                ( Number "11" )
                                                            )
                                                            ( Expression'Case
                                                                ( CaseExpression
                                                                    { base = Just
                                                                        ( Expression'Case
                                                                            ( CaseExpression
                                                                                { base = Nothing
                                                                                , cases =
                                                                                    ( Expression'Not
                                                                                        ( Expression'Not
                                                                                            ( Expression'Between
                                                                                                ( Expression'LiteralValue
                                                                                                    ( Number "17" )
                                                                                                )
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
                                                                                                ( Expression'Multiply
                                                                                                    ( Expression'Divide
                                                                                                        ( Expression'FunctionCall
                                                                                                            ( FunctionCallExpression
                                                                                                                { call = FunctionCall
                                                                                                                    { name = Namespaced
                                                                                                                        { namespace = Nothing
                                                                                                                        , value = "abs"
                                                                                                                        }
                                                                                                                    , arguments = FunctionArguments'Arguments
                                                                                                                        [ Expression'LiteralValue
                                                                                                                            ( Number "19" )
                                                                                                                        ]
                                                                                                                    }
                                                                                                                , filter = Nothing
                                                                                                                , over = Nothing
                                                                                                                }
                                                                                                            )
                                                                                                        )
                                                                                                        ( Expression'FunctionCall
                                                                                                            ( FunctionCallExpression
                                                                                                                { call = FunctionCall
                                                                                                                    { name = Namespaced
                                                                                                                        { namespace = Nothing
                                                                                                                        , value = "abs"
                                                                                                                        }
                                                                                                                    , arguments = FunctionArguments'Arguments
                                                                                                                        [ Expression'Column
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
                                                                                                                        ]
                                                                                                                    }
                                                                                                                , filter = Nothing
                                                                                                                , over = Nothing
                                                                                                                }
                                                                                                            )
                                                                                                        )
                                                                                                    )
                                                                                                    ( Expression'LiteralValue
                                                                                                        ( Number "19" )
                                                                                                    )
                                                                                                )
                                                                                            )
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
                                                                                    ) :|
                                                                                    [
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
                                                                                                                        ( Expression'LiteralValue
                                                                                                                            ( Number "13" )
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
                                                                                        )
                                                                                    ]
                                                                                , else_ = Expression'Negate
                                                                                    ( Expression'Column
                                                                                        ( Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "e"
                                                                                            }
                                                                                        )
                                                                                    )
                                                                                }
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
                                                                                , value = "c"
                                                                                }
                                                                            )
                                                                        ) :| []
                                                                    , else_ = Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "e"
                                                                            }
                                                                        )
                                                                    }
                                                                )
                                                            )
                                                            ( Expression'LiteralValue
                                                                ( Number "17" )
                                                            )
                                                        , Expression'LiteralValue
                                                            ( Number "19" )
                                                        ) :|
                                                        [
                                                            ( Expression'Not
                                                                ( Expression'Or
                                                                    ( Expression'And
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
                                                                                                ( Expression'Equals
                                                                                                    ( Expression'LiteralValue
                                                                                                        ( Number "11" )
                                                                                                    )
                                                                                                    ( Expression'Column
                                                                                                        ( Namespaced
                                                                                                            { namespace = Nothing
                                                                                                            , value = "f"
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
                                                                                                ( Expression'NotEquals
                                                                                                    ( Expression'Column
                                                                                                        ( Namespaced
                                                                                                            { namespace = Nothing
                                                                                                            , value = "d"
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
                                                                    ( Expression'GreaterThanOrEquals
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
                                                                            ( Number "17" )
                                                                        )
                                                                    )
                                                                )
                                                            , Expression'Column
                                                                ( Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "a"
                                                                    }
                                                                )
                                                            )
                                                        ]
                                                    , else_ = Expression'Column
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
                                                    }
                                                )
                                            ) :| []
                                        , else_ = Expression'LiteralValue
                                            ( Number "19" )
                                        }
                                    )
                                )
                                ( Expression'LiteralValue
                                    ( Number "11" )
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
                            ( Expression'Equals
                                ( Expression'Plus
                                    ( Expression'Case
                                        ( CaseExpression
                                            { base = Just
                                                ( Expression'LiteralValue
                                                    ( Number "17" )
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
                                                        , value = "f"
                                                        }
                                                    )
                                                , Expression'LiteralValue
                                                    ( Number "11" )
                                                ) :| []
                                            , else_ = Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "c"
                                                    }
                                                )
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