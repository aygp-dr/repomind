# Claude Code Agents Feature Research

## Overview

This document summarizes research findings about Claude Code's agent features, their introduction timeline, and capabilities.

## Current Version
- Claude Code Version: **1.0.61**
- Status: **General Availability (GA)**

## Agent Feature Timeline

### Key Milestones

1. **Research Preview Phase**
   - Claude Code launched as an agentic command line tool in research preview
   - Core functionality focused on natural language coding assistance

2. **General Availability**
   - Expanded access to Pro and Max plans
   - Introduction of custom subagents capability
   - SDK availability in TypeScript and Python

3. **Custom Subagents Introduction**
   - Users can now create custom subagents for specialized tasks
   - Agents stored in `.claude/agents/` directory
   - Markdown-based agent definitions with YAML frontmatter

### Agent Capabilities

1. **Built-in Agents**
   - `general-purpose`: For researching complex questions and multi-step tasks
   - `feature-builder`: For implementing features, fixing bugs, and improving code

2. **Custom Agent Structure**
   - Name, description, and color theme configuration
   - Specialized prompts and behaviors
   - Integration with Claude Code's Tool system

3. **Agent Invocation**
   - Agents launched via the Task tool
   - Stateless execution model
   - Autonomous task completion

## Technical Implementation

### Agent Definition Format
```yaml
---
name: agent-name
description: Agent description and use cases
color: theme-color
---

[Agent prompt and instructions]
```

### Integration Points
- MCP (Model Context Protocol) support
- OAuth 2.0 authentication for remote servers
- Tool orchestration capabilities

## Related Features

1. **Slash Commands**
   - Custom commands in `.claude/commands/`
   - Markdown-based prompt expansion

2. **Tool Improvements**
   - Progress messages in Bash tool
   - Vim bindings support
   - Enhanced file manipulation

3. **Platform Integration**
   - GitHub Actions support
   - VS Code and JetBrains IDE integration
   - Direct file editing capabilities

## Future Considerations

The agent system appears designed for extensibility, with:
- Custom agent creation capabilities
- SDK support for programmatic interaction
- Integration with existing development workflows

## Sources

- Claude Code GitHub Repository
- Anthropic Documentation
- npm Package Registry (@anthropic-ai/claude-code)
- Community-created agent repositories (e.g., iannuttall/claude-agents)

---
*Research conducted: July 26, 2025*