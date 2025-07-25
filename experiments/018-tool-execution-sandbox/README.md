# Experiment 018: Tool Execution Sandbox

## Overview
Create a secure sandbox environment for executing tools called by the LLM, ensuring safety, resource limits, and proper isolation.

## Goals
- Implement secure execution environment
- Enforce resource limits (CPU, memory, time)
- Provide filesystem and network isolation
- Enable safe tool experimentation
- Comprehensive audit logging

## Success Criteria
- [ ] Tools execute in isolated environment
- [ ] Resource limits enforced effectively
- [ ] Malicious operations prevented
- [ ] Audit trail of all executions
- [ ] Performance overhead < 10%

## Dependencies
- Experiment 017: Tool specification (defines what tools to sandbox)

## Files
- `sandbox-executor.scm` - Main sandbox implementation
- `resource-limiter.scm` - Resource usage controls
- `security-policy.scm` - Access control policies
- `audit-logger.scm` - Execution audit trails

## Running the Experiment
```bash
make test MAX_EXECUTION_TIME=30 MEMORY_LIMIT=512M
```

## Sandbox Security Layers

### Process Isolation
- Separate process for each execution
- No access to parent environment
- Limited system call access
- Signal handling restrictions

### Filesystem Isolation
```
sandbox/
├── workspace/         # Tool working directory
│   └── [session-id]/  # Per-execution isolation
├── readonly/          # Read-only mounted data
└── tmp/              # Temporary scratch space
```

### Resource Limits
- **CPU**: Maximum execution time
- **Memory**: Process memory cap
- **Disk**: Workspace size limit
- **Network**: Disabled by default
- **Processes**: Fork bomb prevention

### Security Policies
```scheme
(define default-policy
  '((filesystem
     (read ("/usr" "/lib" "/bin"))
     (write ("./workspace"))
     (deny ("/" "/etc" "/home")))
    (network
     (allow ())
     (deny all))
    (processes
     (max-children 5)
     (nice-level 10))))
```

## Tool Execution Flow
1. Validate tool request
2. Prepare isolated workspace
3. Apply security policies
4. Execute with resource limits
5. Capture output and errors
6. Clean up workspace
7. Log execution details

## Sandbox Implementation Options

### FreeBSD-specific
- **jail**: Built-in containerization
- **capsicum**: Capability-based security
- **rctl**: Resource controls

### Cross-platform
- **firejail**: Linux sandboxing
- **pledge/unveil**: OpenBSD security
- **Process limits**: POSIX rlimits

## Audit Log Format
```json
{
  "timestamp": "2025-07-25T10:30:00Z",
  "tool": "read_file",
  "parameters": {"path": "README.md"},
  "execution_time": 0.023,
  "memory_peak": 12582912,
  "status": "success",
  "output_size": 2048,
  "policy_violations": []
}
```

## Error Scenarios
- **Timeout**: Execution exceeds time limit
- **OOM**: Memory limit exceeded
- **Access Denied**: Policy violation
- **Resource Exhaustion**: Disk/process limits
- **Signal Termination**: Killed by system

## Performance Considerations
- Sandbox startup overhead
- Memory for isolation
- Cleanup costs
- Parallel execution support

## Results
Status: ⏳ Pending - Critical for secure tool execution