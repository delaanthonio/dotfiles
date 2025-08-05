Analyze workflow performance, agent reliability, and system efficiency metrics to identify optimization opportunities.

Use the Task tool to delegate to the **workflow-monitor** agent with one of these analysis modes:

**Quick Health Check**
```
/workflow-performance --health
```
- Verify all agents are accessible and properly configured
- Check for outdated references or broken interdependencies
- Validate workflow command configurations
- Provide system status overview

**Performance Report**
```
/workflow-performance --report [timeframe]
```
Where timeframe is: `daily`, `weekly`, `monthly`, or `last-N-days`
- Agent success rates and completion times
- Review dispatch efficiency and resource utilization
- Workflow cycle time breakdown and bottleneck identification
- Escalation patterns and blocking scenario analysis

**Optimization Analysis**
```
/workflow-performance --optimize
```
- Identify underutilized or redundant agents
- Suggest workflow parameter adjustments
- Recommend new specialized agents for frequent escalations
- Propose dispatch rule refinements based on usage patterns

**Agent Deep Dive**
```
/workflow-performance --agent [agent-name]
```
- Detailed analysis of specific agent performance
- Success/failure patterns and common blocking scenarios
- Integration effectiveness with other agents
- Optimization recommendations for the target agent

**Comprehensive Analysis**
```
/workflow-performance --full
```
- Complete system analysis combining all modes
- Historical trend analysis and pattern recognition
- Strategic recommendations for workflow evolution
- Risk assessment and mitigation strategies

The workflow-monitor agent will analyze git logs, TodoWrite patterns, agent outputs, and system configurations to provide data-driven insights for continuous improvement of your AI-assisted development workflow.