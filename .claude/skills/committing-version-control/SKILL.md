---
name: committing-version-control
description: Creates git commits with proper authorship, writes clear commit messages, and manages version control. Use when changes need to be committed.
---

# Committing Version Control

## Overview
Manages git commits with proper attribution and clear messaging. Sets appropriate authorship based on who made changes and creates structured commit messages.

## Capabilities

- Create git commits with proper authorship attribution
- Generate clear, structured commit messages
- Stage appropriate files for commits
- Handle git configuration and permissions
- Coordinate push operations with user approval

## How to Use
1. **Analyze Changes**: Review all modified files and understand scope
2. **Set Author**: Choose appropriate git author based on change type
3. **Create Message**: Write concise title and detailed description
4. **Stage Files**: Add relevant files to git staging area
5. **Commit**: Execute git commit with proper attribution
6. **Coordinate**: Handle push operations with user approval

## Author Attribution
Set git authorship based on change source:
```bash
# For codebase related changes (including tests and doc):
git -c user.name="Claude Developer" commit

# For other changes:
git -c user.name="Claude General Manager" commit
```

Never provide an email.

## Commit Message Format

```
🤖✨ Short descriptive title (≤50 chars)

Detailed paragraph explaining:
- What was implemented/changed
- Why the change was necessary
- Important technical details
- Impact on other components
```

The commit message should never contain a footer mentioning Claude Authorship, it is fully implied by  🤖 and user.name.

The 🤖 emoji signals that these were llm made changes and should never be omitted.
The ✨ emoji here is to be replaced approproately according the following lookup table.
We want to avoid commits that combine change types.
An exception is that the plan update directly resulting from a change should be included in the same commit, without mentionning it in the message (e.g. , good: 🤖✨ Add download button, bad : 🤖✨ Add download button and update plan).


| emoji | commit type                         |
| -- | -------------------------------------- |
| ✨ | New functionality                      |
| 🔧 | Bug or issue fix                       |
| 🧹 | Code cleanup, restructuring, moving/renaming files |
| 📚 | Documentation changes                  |
| 🎨 | Formatting or style adjustments        |
| ✅ | Adding or fixing tests                 |


## Message Guidelines
- **Title**: Imperative mood, specific but concise
- **Body**: Explain "why" behind changes, provide context
- **Scope**: Reference plan items when relevant

## Workflow
1. Receive completed work from implementation skill
2. Analyze all changes and determine scope
3. Generate commit message with appropriate title/description
4. Stage files and set proper authorship
5. Execute commit with structured message
6. Coordinate with user for push operations if needed

## Error Handling
- Handle git permission issues gracefully
- Provide clear error messages and solutions
- Fall back to manual commit instructions if needed
- Coordinate with user for authentication issues

## Best Practices
- Keep commits atomic and focused
- Avoid mixing unrelated changes
- Ensure commits build successfully
- Use consistent messaging style
- Coordinate timing for collaborative workflows

Focus on creating clean, well-documented git history that clearly shows the evolution of the project.