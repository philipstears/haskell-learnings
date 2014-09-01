fromMaybe defaultValue actualValue =
    case actualValue of
        Nothing -> defaultValue
        Just innerValue -> innerValue
