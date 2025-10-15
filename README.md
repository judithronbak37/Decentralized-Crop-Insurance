# 🌾 Decentralized Crop Insurance Smart Contract

A blockchain-based crop insurance system that provides automated payouts to farmers based on weather data from authorized oracles. Farmers pay premiums and receive coverage against crop failures due to floods, droughts, and extreme weather conditions.

## 🚀 Features

- **Premium-based Insurance**: Farmers pay premiums to get crop coverage
- **Oracle Integration**: Authorized oracles submit weather data to trigger claims
- **Automated Payouts**: Smart contract automatically processes claims based on weather conditions
- **Multiple Risk Coverage**: Protects against floods, droughts, and extreme weather
- **Transparent Pool Management**: Community-funded insurance pool with clear statistics

## 📋 Contract Functions

### 🔐 Admin Functions
- `add-oracle` - Add authorized weather data oracle
- `remove-oracle` - Remove oracle authorization
- `withdraw-excess-funds` - Withdraw surplus funds from insurance pool

### 👨‍🌾 Farmer Functions
- `create-policy` - Purchase crop insurance policy
- `process-claim` - Process triggered insurance claim

### 🌤️ Oracle Functions
- `submit-weather-report` - Submit weather data for specific location
- `manual-trigger-claim` - Manually trigger claim based on weather conditions

### 💰 Pool Functions
- `fund-insurance-pool` - Add funds to insurance pool

### 📊 Read-Only Functions
- `get-policy` - Get policy details by ID
- `get-farmer-policies` - Get all policies for a farmer
- `get-weather-report` - Get weather data for location and block
- `get-claim-trigger` - Get claim trigger information
- `get-insurance-pool-balance` - Get current pool balance
- `get-contract-stats` - Get overall contract statistics
- `is-oracle-authorized` - Check if oracle is authorized

## 🛠️ Usage Instructions

### Deploy Contract
```bash
clarinet deploy
```

### Create Insurance Policy
```clarity
(contract-call? .decentralized-crop-insurance create-policy 
  u1000000  ;; premium amount (1 STX)
  u5000000  ;; coverage amount (5 STX)
  "corn"    ;; crop type
  "iowa-farm-district-1"  ;; location
  u52560)   ;; duration in blocks (~1 year)
```
