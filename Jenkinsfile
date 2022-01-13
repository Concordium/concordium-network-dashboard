@Library('concordium-pipelines') _
pipeline {
    agent any
    environment {
        ecr_repo_domain = '192549843005.dkr.ecr.eu-west-1.amazonaws.com'
        image_repo = "${ecr_repo_domain}/concordium/network-dashboard"
        image_name = "${image_repo}:${image_tag}"
        min_version_included_in_stats = "${minimum_node_version}"
    }
    stages {
        stage('ecr-login') {
            steps {
                ecrLogin(env.ecr_repo_domain, 'eu-west-1')
            }
        }
        stage('build') {
            steps {
                sh '''\
                  docker build --build-arg min_version_included_in_stats=${min_version_included_in_stats} --label min_version_included_in_stats=${min_version_included_in_stats} -t "${image_name}" -f k8s.Dockerfile .
                  docker push "${image_name}"
                '''
            }
        }
    }
}
