# I configure the infrastructure for a static website:
# * Sourced from a Gitlab repository
# * Hosted from Netlify
# * Viewed from a Cloudflare subdomain
# * Continuously built/deployed on git-push
#
# Run me with `terraform apply`. Maybe do a `terraform plan` first, too.
#
# To use me in a new project, just copy this file and change the `locals` block.

terraform {
  backend "s3" {
    bucket = "terraform-backend.erosson.org"
    key    = "math.swarmsim.com"
    region = "us-east-1"
  }
}

provider "cloudflare" {
  version = "~> 1.2"
}

provider "gitlab" {
  version = "~> 1.0"
}

provider "netlify" {
  version = "~> 0.1"
}

locals {
  project    = "math"
  hostdomain = "swarmsim.com"
  fulldomain = "${local.project}.${local.hostdomain}"
}

resource "gitlab_project" "git" {
  #name             = "${local.project}"
  name             = "${local.project}-swarmsim-com"
  description      = "https://${local.fulldomain}"
  visibility_level = "public"
  default_branch   = "master"

  provisioner "local-exec" {
    command = <<EOF
sh -eu
git remote remove origin || true
git remote add origin ${gitlab_project.git.ssh_url_to_repo}
git push -u origin master
EOF
  }
}

module "webhost" {
  source = "git::ssh://git@gitlab.com/erosson/terraform.git//netlify/gitlab"
  name = "${gitlab_project.git.name}"
  custom_domain = "${local.fulldomain}"

  repo {
    repo_branch = "master"
    command     = "yarn build:ci"
    dir         = "build"
    repo_path   = "erosson/${gitlab_project.git.name}"
  }
}

resource "cloudflare_record" "dns" {
  domain  = "${local.hostdomain}"
  name    = "${local.project}"
  type    = "CNAME"
  value   = "${module.webhost.dns}"
  proxied = false                   # netlify does its own proxying
}
