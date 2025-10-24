# 🛡️ Trustless Escrow for Cross-Border Freelancing

A decentralized escrow smart contract built on Stacks blockchain that enables secure payments between clients and freelancers worldwide without intermediaries.

## 🚀 Features

- **Secure Escrow System**: Funds are held in contract until work completion
- **Automated Payments**: Smart contract releases funds upon work approval
- **Dispute Resolution**: Built-in arbitration system with evidence submission
- **User Rating System**: Track reputation and completed projects
- **Deadline Management**: Time-based protections for both parties
- **Low Fees**: Minimal contract fees (2.5% default)
- **Cross-Border Ready**: Works globally with STX tokens

## 📋 How It Works

### 1. Create Escrow 💼
```clarity
(create-escrow freelancer-address amount deadline-block "Work description")
```
- Client deposits STX tokens
- Sets deadline and work description
- Funds are locked in contract

### 2. Complete Work ✅
```clarity
(complete-work escrow-id)
```
- Freelancer marks work as completed
- Client has 1008 blocks (~1 week) to review

### 3. Approve & Pay 💰
```clarity
(approve-work escrow-id)
```
- Client approves and releases funds
- Contract fee (2.5%) deducted
- Both parties get positive ratings

### 4. Auto-Payment 🕐
```clarity
(claim-expired-escrow escrow-id)
```
- Freelancer can claim payment after 1 week if no response
- Automatic payment for completed work

## 🔧 Usage Instructions

### Prerequisites
- Clarinet CLI installed
- STX tokens for transactions
- Stacks wallet

### Deploy Contract
```bash
clarinet deploy
```

### Testing
```bash
clarinet test
```

## 📖 Contract Functions

### Public Functions

| Function | Description | Parameters |
|----------|-------------|------------|
| `create-escrow` | Create new escrow contract | freelancer, amount, deadline, description |
| `complete-work` | Mark work as completed | escrow-id |
| `approve-work` | Approve and release payment | escrow-id |
| `dispute-escrow` | Initiate dispute process | escrow-id, reason |
| `submit-evidence` | Submit dispute evidence | escrow-id, evidence |
| `resolve-dispute` | Resolve dispute (owner only) | escrow-id, resolution |
| `cancel-escrow` | Cancel expired escrow | escrow-id |
| `claim-expired-escrow` | Claim payment after deadline | escrow-id |

### Read-Only Functions

| Function | Description | Returns |
|----------|-------------|---------|
| `get-escrow` | Get escrow details | Escrow data |
| `get-dispute` | Get dispute information | Dispute data |
| `get-user-rating` | Get user rating stats | Rating data |
| `get-user-average-rating` | Get average rating | Average score |
| `get-contract-balance` | Get contract STX balance | Balance amount |

## 🛡️ Security Features

- **Multi-signature protection**: Both parties must interact for completion
- **Time-locked funds**: Automatic release mechanisms prevent fund locking
- **Dispute resolution**: Owner-mediated dispute system
- **Evidence system**: Both parties can submit proof during disputes
- **Rating system**: Reputation tracking prevents bad actors

## 💡 Example Usage

### Creating an Escrow
```clarity
;; Client creates escrow for 1000 STX with 1000 block deadline
(create-escrow 'SP2FREELANCER123 u1000000000 u1000 "Website development project")
```

### Freelancer Workflow
```clarity
;; Mark work complete
(complete-work u1)

;; If client doesn't respond after deadline
(claim-expired-escrow u1)
```

### Client Workflow
```clarity
;; Approve completed work
(approve-work u1)

;; Or dispute if unsatisfied
(dispute-escrow u1 "Work doesn't meet requirements")
```

## ⚙️ Configuration

### Contract Parameters
- **Dispute Fee**: 1 STX (prevents spam disputes)
- **Contract Fee**: 2.5% of escrow amount
- **Auto-payment Window**: 1008 blocks (~1 week)

### Admin Functions
```clarity
;; Update dispute fee (owner only)
(set-dispute-fee u2000000)

;; Update contract fee rate (owner only, max 10%)
(set-contract-fee-rate u300)
```

## 🔍 Dispute Resolution Process

1. **Initiate Dispute** 🚨
   - Either party can dispute
   - Must pay dispute fee
   - Provide initial reason

2. **Submit Evidence** 📋
   - Both parties submit evidence
   - 500 character limit per submission
   - Multiple evidence submissions allowed

3. **Resolution** ⚖️
   - Contract owner reviews evidence
   - Decides: "client", "freelancer", or "split"
   - Funds distributed accordingly
   - Dispute fee returned to winner

## 🌟 Benefits

- **No Intermediaries**: Direct peer-to-peer escrow
- **Global Access**: Works anywhere with internet
- **Low Costs**: Only 2.5% contract fee
- **Transparency**: All transactions on blockchain
- **Security**: Smart contract handles all logic
- **Reputation**: Built-in rating system

## 🚨 Important Notes

- Always verify escrow details before funding
- Keep track of deadlines and respond promptly
- Dispute fees are non-refundable if you lose
- Contract owner can resolve disputes
- Auto-payment activates after 1 week of completion

## 📞 Support

For technical issues or questions about the contract:
- Check the contract code in `contracts/Trustless-escrow.clar`
- Review test files for usage examples
- Submit GitHub issues for bugs or improvements

---

**Built with ❤️ for the global freelance community** 🌍
