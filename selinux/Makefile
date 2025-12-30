# SPDX-License-Identifier: AGPL-3.0-or-later
# Makefile for SELinux policy module

POLICY_NAME = cloud_sync_tuner
POLICY_VERSION = 1.0.0

# Find selinux devel files
SELINUX_SHARE = $(shell pkg-config --variable=makedir selinux-policy-devel 2>/dev/null || echo /usr/share/selinux/devel)

all: $(POLICY_NAME).pp

$(POLICY_NAME).pp: $(POLICY_NAME).te $(POLICY_NAME).fc $(POLICY_NAME).if
	$(MAKE) -f $(SELINUX_SHARE)/Makefile $(POLICY_NAME).pp

install: $(POLICY_NAME).pp
	semodule -i $(POLICY_NAME).pp
	restorecon -R -v /home/*/\.local/bin/rclone || true
	restorecon -R -v /home/*/\.config/rclone || true
	restorecon -R -v /home/*/\.config/cloud-sync-tuner || true
	restorecon -R -v /home/*/\.cache/rclone || true

uninstall:
	semodule -r $(POLICY_NAME) || true

clean:
	rm -f $(POLICY_NAME).pp
	rm -rf tmp/

check:
	checkmodule -M -m -o $(POLICY_NAME).mod $(POLICY_NAME).te
	semodule_package -o $(POLICY_NAME).pp -m $(POLICY_NAME).mod -f $(POLICY_NAME).fc

.PHONY: all install uninstall clean check
